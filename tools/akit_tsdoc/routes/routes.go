package routes

import (
	"embed"

	"github.com/gin-gonic/gin"
)

// SetupRoutes 设置所有路由
func SetupRoutes(r *gin.Engine, content embed.FS) {
	r.GET("/", func(c *gin.Context) {
		showIndex(c, content)
	})

	r.POST("/upload", uploadImage)

	r.POST("/export", exportImages)
}
