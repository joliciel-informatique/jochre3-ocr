package com.joliciel.jochre.ocr.core.model

case class JochreImage(
    id: String,
    height: Int,
    width: Int,
    physicalPageNumber: Int,
    rotation: Double,
    textBlocks: Seq[TextBlock],
    illustrations: Seq[Illustration])