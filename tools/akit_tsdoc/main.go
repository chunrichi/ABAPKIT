package main

import (
	"embed"
	"html/template"
	"os"
	"tsdoc/routes"

	"github.com/gin-gonic/gin"
)

//go:embed web/templates/*
var tmpl embed.FS

func main() {
	r := gin.Default()

	// 载入模板
	htmTmpl, err := template.ParseFS(tmpl, "web/templates/*.html")
	if err != nil {
		panic(err)
	}

	r.SetHTMLTemplate(htmTmpl)

	// 载入路由
	routes.SetupRoutes(r)

	// 检查 uploads 目录是否存在，如果不存在则创建
	if _, err := os.Stat("uploads"); os.IsNotExist(err) {
		os.Mkdir("uploads", os.ModePerm)
	}

	r.Run(":8080")
}
