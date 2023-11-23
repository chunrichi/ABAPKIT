package routes

import (
	"gorm.io/driver/sqlite"
	"gorm.io/gorm"
)

// 数据库连接
var DB *gorm.DB

// 评论图片
type CommentsImg struct {
	gorm.Model
	ImagePath string
	Comment   string
}

// 初始化
func init() {
	var err error
	DB, err = gorm.Open(sqlite.Open("./test.db"), &gorm.Config{})
	if err != nil {
		panic("Failed to connect database")
	}

	err = DB.AutoMigrate(&CommentsImg{})
	if err != nil {
		panic("Migrate table failed")
	}

}
