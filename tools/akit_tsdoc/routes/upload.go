package routes

import (
	"fmt"
	"io"
	"net/http"
	"os"
	"path/filepath"
	"sync"

	"github.com/gin-gonic/gin"
)

// 互斥锁
var mu sync.Mutex

// 评论和图片关联的map
var comments = make(map[string]string)

func uploadImage(c *gin.Context) {
	mu.Lock()
	defer mu.Unlock()

	comment := c.PostForm("comment")

	// 检查是否有通过Ctrl+V粘贴的图片
	fileHeader, err := c.FormFile("image")
	if err != nil {
		// 如果没有通过Ctrl+V粘贴的图片，则尝试从request body中读取
		fileHeader, err = c.FormFile("image[]")
		if err != nil {
			c.JSON(http.StatusInternalServerError, gin.H{"error": "No image file provided"})
			return
		}
	}

	// 处理上传的文件
	file, err := fileHeader.Open()
	if err != nil {
		c.JSON(http.StatusInternalServerError, gin.H{"error": err.Error()})
		return
	}
	defer file.Close()

	// 生成唯一的文件名
	filename := fmt.Sprintf("%d_%s", fileHeader.Size, fileHeader.Filename)

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
}

func showIndex(c *gin.Context) {
	mu.Lock()
	defer mu.Unlock()

	// 获取所有评论和图片
	data := struct {
		Comments map[string]string
	}{
		Comments: comments,
	}

	// 渲染HTML模板
	c.HTML(200, "index.html", data)
}
