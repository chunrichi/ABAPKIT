package main

import (
	"encoding/csv"
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"sync"
)

type Config struct {
	FolderPaths  []string `json:"folderPaths"`
	RegexPattern string   `json:"regexPattern"`
	OutputFile   string   `json:"outputFile"`
	IncludeDirs  []string `json:"includeDirs"`
}

// MatchResult 结构用于表示匹配结果
type MatchResult struct {
	FilePath    string
	MatchedData string
	Success     bool
}

// ProgressBar 结构用于进度展示
type ProgressBar struct {
	FolderIndex int
	Message     string
}

func (pb ProgressBar) Print(indexNow int) {
	calcIndex := indexNow - pb.FolderIndex

	// \033[nA 上移动n行 B 下移

	if calcIndex > 0 {
		fmt.Printf("\r\033[%vA%s", calcIndex, pb.Message)
	} else if calcIndex < 0 {
		fmt.Printf("\r\033[%vB%s", -calcIndex, pb.Message)
	} else {
		fmt.Printf("\r%s", pb.Message)
	}

}

func main() {
	print("\tABAP Scaner\n")

	// 读取配置文件
	config, err := readConfig("config.json")
	if err != nil {
		fmt.Println("Error reading config file:", err)
		return
	}

	// 创建等待数组，用于等待所有 goroutine 完成
	var wg sync.WaitGroup

	// 创建通道 用于接收进度信息
	progressChan := make(chan ProgressBar)
	// 创建通道 用于接收匹配结果
	matchesChan := make(chan MatchResult)

	// 打开 CSV 文件用于写入
	csvFile, err := os.Create(config.OutputFile)
	if err != nil {
		fmt.Println("Error creating CSV file:", err)
		return
	}
	defer csvFile.Close()

	// 创建 CSV Writer
	csvWriter := csv.NewWriter(csvFile)
	defer csvWriter.Flush()

	// 写入 CSV 标题行
	csvWriter.Write([]string{"File Path", "Matched Data"})

	// 遍历所有文件夹路径，启动异步处理
	for folderIndex, folderPath := range config.FolderPaths {
		wg.Add(1)
		fmt.Printf("Folder: %-50s\n", folderPath)
		go processFolder(&wg, folderIndex, folderPath, config.RegexPattern, config.IncludeDirs, progressChan, matchesChan)
	}

	nowIndex := len(config.FolderPaths)

	// 启动goroutine，监听进度信息
	var wgProgress sync.WaitGroup
	go func() {
		defer wgProgress.Done()
		// 当前索引
		for progress := range progressChan {
			progress.Print(nowIndex)
			nowIndex = progress.FolderIndex
		}
	}()
	wgProgress.Add(1)

	// 启动goroutine，处理匹配结果
	go func() {
		for matchResult := range matchesChan {
			if matchResult.Success {
				// 写入匹配到的数据到 CSV 文件
				csvWriter.Write([]string{matchResult.FilePath, matchResult.MatchedData})
				csvWriter.Flush() // 立即刷新缓冲区以确保进度实时显示
			}
		}
	}()

	// 等待所有goroutine完成
	wg.Wait()

	// 关闭进度通道和匹配结果通道
	close(progressChan)
	close(matchesChan)

	wgProgress.Wait()

	// 重置到最底层
	ProgressBar{FolderIndex: len(config.FolderPaths) + 1, Message: ""}.Print(nowIndex)

	fmt.Println("Task completed successfully.")
}

func readConfig(configPath string) (*Config, error) {
	configFile, err := os.ReadFile(configPath)
	if err != nil {
		return nil, err
	}

	var config Config
	err = json.Unmarshal(configFile, &config)
	if err != nil {
		return nil, err
	}

	return &config, nil
}

func processFolder(wg *sync.WaitGroup, folderIndex int, folderPath, regexPattern string, includeDirs []string, progressChan chan ProgressBar, matchesChan chan MatchResult) {
	defer wg.Done()

	// 获取目录中的所有文件
	filePaths, err := getAllFiles(folderPath)
	if err != nil {
		progressChan <- ProgressBar{FolderIndex: folderIndex,
			Message: fmt.Sprintf("Error getting file paths in %s: %v", folderPath, err)}
		return
	}

	// 计算总文件数量，用于显示进度信息
	totalFiles := len(filePaths)

	// 遍历所有文件，进行正则匹配并写入 CSV 文件
	for i, filePath := range filePaths {
		if includeDirs != nil {
			// 获取相对路径
			relPath, err := filepath.Rel(folderPath, filePath)
			if err != nil {
				progressChan <- ProgressBar{FolderIndex: folderIndex,
					Message: fmt.Sprintf("Error getting relative path for %s: %v", filePath, err)}
				continue
			}

			// 检查是否在特定子目录中
			inTargetDir := false
			for _, targetDir := range includeDirs {
				if strings.HasPrefix(relPath, targetDir) {
					inTargetDir = true
					break
				}
			}

			if !inTargetDir {
				// 如果不在特定子目录中，跳过处理
				continue
			}
		}

		content, err := os.ReadFile(filePath)
		if err != nil {
			progressChan <- ProgressBar{FolderIndex: folderIndex,
				Message: fmt.Sprintf("Error reading file %s: %v", filePath, err)}
			continue
		}

		// 正则匹配
		matches := findMatches(regexPattern, string(content))
		for _, match := range matches {
			// 发送匹配结果
			matchesChan <- MatchResult{FilePath: filePath, MatchedData: match, Success: true}
		}

		// 发送进度信息
		progressChan <- ProgressBar{FolderIndex: folderIndex,
			Message: fmt.Sprintf("Folder: %-50s\t, Progress: %d/%d", folderPath, i+1, totalFiles)}
	}

	// 发送完成信息
	progressChan <- ProgressBar{FolderIndex: folderIndex,
		Message: fmt.Sprintf("Folder: %-50s\t, Completed! %10s", folderPath, "")}
}

// getAllFiles 用于获取指定目录下的所有文件
func getAllFiles(folderPath string) ([]string, error) {
	var filePaths []string

	err := filepath.Walk(folderPath, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}
		if !info.IsDir() {
			filePaths = append(filePaths, path)
		}
		return nil
	})

	if err != nil {
		return nil, err
	}

	return filePaths, nil
}

// findMatches 用于正则匹配
func findMatches(pattern, content string) []string {
	re := regexp.MustCompile(pattern)
	// 忽略大小写在配置中设置 (?i)
	matches := re.FindAllString(content, -1)
	return matches
}

func max(a int32, b int32) int32 {
	if a > b {
		return a
	}
	return b
}
