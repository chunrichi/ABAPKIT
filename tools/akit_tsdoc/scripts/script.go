package scripts

import (
	"os"
	models "tsdoc/models"
)

// 处理参数
func Process(args []string) {
	if args[0] == "fake" {
		models.DB.Create(&models.CommentsImg{ImagePath: "fake.png", Comment: "fake"})
		os.Exit(0)
	} else if args[0] == "drop" {
		models.DB.Unscoped().Exec("DELETE FROM comments_imgs;")
		os.Exit(0)
	} else {
		os.Exit(3)
	}
}
