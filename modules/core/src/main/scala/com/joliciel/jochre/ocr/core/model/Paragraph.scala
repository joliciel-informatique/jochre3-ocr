package com.joliciel.jochre.ocr.core.model

import com.joliciel.jochre.ocr.core.model.ImageLabel.Rectangle

case class Paragraph(rows: Seq[Row], rectangle: Rectangle)
