package main

import (
	"embed"
	"os"
	"tsdoc/routes"

	"github.com/gin-gonic/gin"
)

//go:embed web/templates/*
var content embed.FS

func main() {
	r := gin.Default()

	// 载入路由
	routes.SetupRoutes(r, content)

	// 检查 uploads 目录是否存在，如果不存在则创建
	if _, err := os.Stat("uploads"); os.IsNotExist(err) {
		os.Mkdir("uploads", os.ModePerm)
	}

	r.Run(":8080")
}
