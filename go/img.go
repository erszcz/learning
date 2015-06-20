package main

import (
	"image"
	"image/color"
	"tour/pic"
)

type Image struct{
	cols int
	rows int
	data [][]color.RGBA
}

func (i *Image) ColorModel() color.Model {
	return color.RGBAModel
}

func (i *Image) Bounds() image.Rectangle {
	return image.Rect(0, 0, i.cols, i.rows)
}

func (i *Image) At(x, y int) color.Color {
	return color.RGBA{128,255,128,255}
}

func main() {
	m := new(Image)
	pic.ShowImage(m)
}
