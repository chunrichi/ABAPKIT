package routes

import (
	"net/http"
	models "tsdoc/models"

	"github.com/gin-gonic/gin"
)

func loadImageList(c *gin.Context) {
	ci := make([]models.CommentsImg, 0)
	models.DB.Find(&ci)

	c.JSON(http.StatusOK, gin.H{
		"list": ci,
	})
}
