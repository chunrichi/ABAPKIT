package main

import (
	"embed"
	"html/template"
	"net/http"
	"os"
	"tsdoc/routes"
	"tsdoc/scripts"

	"github.com/gin-gonic/gin"
)

//go:embed web/templates/*
var tmpl embed.FS

func main() {

	if len(os.Args) >= 2 {
		// 带参数运行
		scripts.Process(os.Args[1:])
	}

	r := gin.Default()

	// 载入模板
	htmTmpl, err := template.ParseFS(tmpl, "web/templates/*.html")
	if err != nil {
		panic(err)
	}

	r.SetHTMLTemplate(htmTmpl)

	// 设置静态资源目录
	r.StaticFS("/images", http.Dir("./uploads"))

	// 载入路由
	routes.SetupRoutes(r)

	// 检查 uploads 目录是否存在，如果不存在则创建
	if _, err := os.Stat("uploads"); os.IsNotExist(err) {
		os.Mkdir("uploads", os.ModePerm)
	}

	r.Run(":8080")
}
