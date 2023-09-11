package com.joliciel.jochre.ocr.core.model

import scala.xml.{Elem, Node}

case class Page(
  id: String,
  height: Int,
  width: Int,
  physicalPageNumber: Int,
  rotation: Double,
  blocks: Seq[Block]) extends PageElement {
  override def translate(xDiff: Int, yDiff: Int): Page = {
    this.copy(blocks = blocks.map(_.translate(xDiff, yDiff)).collect{
      case block: Block => block
    })
  }

  def rotate(): Page = {
    this.rotate(ImageInfo(width, height, 0-rotation))
  }

  override def rotate(imageInfo: ImageInfo): Page = {
    this.copy(blocks = blocks.map(_.rotate(imageInfo)).collect {
      case block: Block => block
    })
  }

  override def toXml(idToIgnore: String): Elem =
    <Page ID={id} HEIGHT={height.toString} WIDTH={width.toString} PHYSICAL_IMG_NR={physicalPageNumber.toString} ROTATION={rotation.toString} LANG="yi" PC="0.0000">
      <PrintSpace HEIGHT={height.toString} WIDTH={width.toString} HPOS="0" VPOS="0">
        {blocks.zipWithIndex.map{ case (block, i) => block.toXml(f"${id}_B${i}")}}
      </PrintSpace>
    </Page>
}

object Page {
  def fromXML(node: Node): Page = {
    val page = (node \\ "Page").head
    val id = page \@ "ID"
    val height = (page \@ "HEIGHT").toInt
    val width = (page \@ "WIDTH").toInt
    val physicalPageNumber = (page \@ "PHYSICAL_IMG_NR").toIntOption.getOrElse(0)
    val rotation = (page \@ "ROTATION").toDouble
    val imageInfo = ImageInfo(width, height, rotation)
    val printSpace = (page \ "PrintSpace").head

    val blocks = printSpace.child.collect{
      case elem: Elem if elem.label == "TextBlock" => TextBlock.fromXML(imageInfo, elem)
      case elem: Elem if elem.label == "ComposedBlock" => ComposedBlock.fromXML(imageInfo, elem)
      case elem: Elem if elem.label == "Illustration" => Illustration.fromXML(elem)
    }.toSeq

    Page(id, height, width, physicalPageNumber, rotation, blocks)
  }
}