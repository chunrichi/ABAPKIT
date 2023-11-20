package main

import (
	"encoding/csv"
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"sync"
)

type Config struct {
	FolderPaths  []string `json:"folderPaths"`
	RegexPattern string   `json:"regexPattern"`
	OutputFile   string   `json:"outputFile"`
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

	// 创建通道用于接收进度信息
	progressChan := make(chan string)

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

	// 便利所有文件夹路径，启动异步处理
	for _, folderPath := range config.FolderPaths {
		wg.Add(1)
		go processFolder(&wg, folderPath, config.RegexPattern, csvWriter, progressChan)
	}

	// 启动goroutine，监听进度信息
	go func() {
		for progress := range progressChan {
			fmt.Println("Progress:", progress)
		}
	}()

	// 等待所有goroutine完成
	wg.Wait()

	// 关闭进度通道
	close(progressChan)

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

func processFolder(wg *sync.WaitGroup, folderPath, regexPattern string, csvWriter *csv.Writer, progressChan chan string) {
	defer wg.Done()

	// 获取目录中的所有文件
	filePaths, err := getAllFiles(folderPath)
	if err != nil {
		progressChan <- fmt.Sprintf("Error getting file paths in %s: %v", folderPath, err)
		return
	}

	// 计算总文件数量，用于显示进度信息
	totalFiles := len(filePaths)

	// 遍历所有文件，进行正则匹配并写入 CSV 文件
	for i, filePath := range filePaths {
		content, err := os.ReadFile(filePath)
		if err != nil {
			progressChan <- fmt.Sprintf("Error reading file %s: %v", filePath, err)
			continue
		}

		// 正则匹配
		matches := findMatches(regexPattern, string(content))
		if len(matches) > 0 {
			// 写入匹配到的数据到 CSV 文件
			csvWriter.Write([]string{filePath, matches[0]}) // 这里只写入了第一个匹配项，你可以根据实际需求调整
		}

		// 发送进度信息
		progressChan <- fmt.Sprintf("Folder: %s, Progress: %d/%d", folderPath, i+1, totalFiles)
	}

	// 发送完成信息
	progressChan <- fmt.Sprintf("Folder: %s, Completed", folderPath)
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
	matches := re.FindStringSubmatch(content)
	return matches
}
