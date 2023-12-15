package routes

import (
	"github.com/gin-gonic/gin"
)

// SetupRoutes 设置所有路由
func SetupRoutes(r *gin.Engine) {
	r.GET("/", showIndex)

	r.POST("/upload", uploadImage)

	r.POST("/export", exportImages)
	r.POST("/imagesList", loadImageList)
}
