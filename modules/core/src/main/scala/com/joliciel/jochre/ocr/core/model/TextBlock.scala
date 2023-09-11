package com.joliciel.jochre.ocr.core.model

import com.joliciel.jochre.ocr.core.model.ImageLabel.Rectangle

case class TextBlock(paragraphs: Seq[Paragraph], rectangle: Rectangle)
