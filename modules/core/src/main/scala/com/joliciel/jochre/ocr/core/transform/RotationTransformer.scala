package com.joliciel.jochre.ocr.core.transform

import com.joliciel.jochre.ocr.core.utils.OpenCvUtils
import org.bytedeco.opencv.opencv_core.Mat
import org.slf4j.LoggerFactory
import org.w3c.dom.{Attr, NodeList}

import javax.xml.parsers.DocumentBuilderFactory
import javax.xml.xpath.{XPathConstants, XPathFactory}

class RotationTransformer(val defaultAngle: Double = 0.0) extends ImageTransformer[Double] with OpenCvUtils {
  private val log = LoggerFactory.getLogger(getClass)

  override def transform(path: String, mat: Mat): (Mat, Double) = {
    val angle = this.getRotation(path)
    this.unrotate(angle, mat) -> angle
  }

  def getRotation(path: String): Double = {
    val altoPath = path.substring(0, path.lastIndexOf('.')) + "_alto4.xml"
    val builderFactory = DocumentBuilderFactory.newInstance
    val builder = builderFactory.newDocumentBuilder
    val xmlDocument = builder.parse(altoPath)
    val xPath = XPathFactory.newInstance.newXPath
    val xPathExpression = xPath.compile("//@ROTATION")
    val nodeList = xPathExpression.evaluate(xmlDocument, XPathConstants.NODESET).asInstanceOf[NodeList]
    val attribute = nodeList.item(0).asInstanceOf[Attr]
    val rotation = attribute.getValue.toDouble
    if (log.isDebugEnabled) log.debug("For file " + altoPath + ", Rotation: " + rotation)
    rotation
  }
}
