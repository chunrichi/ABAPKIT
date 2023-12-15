package routes

import (
	"archive/zip"
	"fmt"
	"io"
	"os"
	"path/filepath"

	"github.com/gin-gonic/gin"
)

func exportImages(c *gin.Context) {
	go func() {
		mu.Lock()
		defer mu.Unlock()

		// 创建一个临时目录用于存放图片文件
		tempDir := "temp"
		os.Mkdir(tempDir, os.ModePerm)
		defer os.RemoveAll(tempDir)

		// 将所有图片复制到临时目录
		for filename := range comments {
			sourcePath := filepath.Join("uploads", filename)
			destPath := filepath.Join(tempDir, filename)

			source, err := os.Open(sourcePath)
			if err != nil {
				fmt.Println("Error opening source file:", err)
				continue
			}
			defer source.Close()

			dest, err := os.Create(destPath)
			if err != nil {
				fmt.Println("Error creating destination file:", err)
				continue
			}
			defer dest.Close()

			_, err = io.Copy(dest, source)
			if err != nil {
				fmt.Println("Error copying file:", err)
				continue
			}
		}

		// 打包为压缩文件
		zipFilename := "exported_images.zip"
		zipFile, err := os.Create(zipFilename)
		if err != nil {
			fmt.Println("Error creating zip file:", err)
			return
		}
		defer zipFile.Close()

		zipWriter := zip.NewWriter(zipFile)
		defer zipWriter.Close()

		filepath.Walk(tempDir, func(filePath string, info os.FileInfo, err error) error {
			if err != nil {
				fmt.Println("Error walking directory:", err)
				return err
			}

			if !info.IsDir() {
				relPath, err := filepath.Rel(tempDir, filePath)
				if err != nil {
					fmt.Println("Error getting relative path:", err)
					return err
				}

				zipEntry, err := zipWriter.Create(relPath)
				if err != nil {
					fmt.Println("Error creating zip entry:", err)
					return err
				}

				file, err := os.Open(filePath)
				if err != nil {
					fmt.Println("Error opening file:", err)
					return err
				}
				defer file.Close()

				_, err = io.Copy(zipEntry, file)
				if err != nil {
					fmt.Println("Error copying file to zip:", err)
					return err
				}
			}

			return nil
		})
	}()
}
