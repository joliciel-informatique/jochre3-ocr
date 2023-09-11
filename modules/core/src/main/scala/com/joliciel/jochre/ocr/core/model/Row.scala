package com.joliciel.jochre.ocr.core.model

import com.joliciel.jochre.ocr.core.model.ImageLabel.Line

case class Row(words: Seq[Word], baseLine: Line)
