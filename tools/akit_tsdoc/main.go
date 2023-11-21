package main

import (
	"archive/zip"
	"embed"
	"fmt"
	"html/template"
	"io"
	"net/http"
	"os"
	"path/filepath"
	"sync"

	"github.com/gin-gonic/gin"
)

//go:embed web/templates/*
var content embed.FS

var (
	mu       sync.Mutex
	comments = make(map[string]string)
)

func main() {
	r := gin.Default()

	// 设置嵌入的HTML模板
	htmlTemplate, err := template.ParseFS(content, "web/templates/*.html")
	if err != nil {
		panic(err)
	}

	// 首页路由
	r.GET("/", func(c *gin.Context) {
		mu.Lock()
		defer mu.Unlock()

		// 获取所有评论和图片
		data := struct {
			Comments map[string]string
		}{
			Comments: comments,
		}

		// 渲染HTML模板
		htmlTemplate.ExecuteTemplate(c.Writer, "index.html", data)
	})

	// 上传图片和评论的路由
	r.POST("/upload", func(c *gin.Context) {
		mu.Lock()
		defer mu.Unlock()

		comment := c.PostForm("comment")

		// 处理上传的文件
		file, header, err := c.Request.FormFile("image")
		if err != nil {
			c.JSON(http.StatusInternalServerError, gin.H{"error": err.Error()})
			return
		}
		defer file.Close()

		// 生成唯一的文件名
		filename := fmt.Sprintf("%d_%s", header.Size, header.Filename)

		// 保存文件到服务器
		out, err := os.Create(filepath.Join("uploads", filename))
		if err != nil {
			c.JSON(http.StatusInternalServerError, gin.H{"error": err.Error()})
			return
		}
		defer out.Close()
		_, err = io.Copy(out, file)
		if err != nil {
			c.JSON(http.StatusInternalServerError, gin.H{"error": err.Error()})
			return
		}

		// 关联评论和图片
		comments[filename] = comment

		c.Redirect(http.StatusSeeOther, "/")
	})

	// 执行任务并打包为压缩文件的路由
	r.GET("/export", func(c *gin.Context) {
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
	})

	// 检查 uploads 目录是否存在，如果不存在则创建
	if _, err := os.Stat("uploads"); os.IsNotExist(err) {
		os.Mkdir("uploads", os.ModePerm)
	}

	r.Run(":8080")
}
