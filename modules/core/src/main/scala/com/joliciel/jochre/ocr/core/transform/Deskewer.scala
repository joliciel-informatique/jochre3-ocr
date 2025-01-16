package com.joliciel.jochre.ocr.core.transform

import com.joliciel.jochre.ocr.core.corpus.AltoFinder
import com.joliciel.jochre.ocr.core.graphics.Rectangle
import com.joliciel.jochre.ocr.core.utils.{FileUtils, ImageUtils, MathUtils}
import com.typesafe.config.ConfigFactory
import org.bytedeco.opencv.global.opencv_core._
import org.bytedeco.opencv.global.opencv_imgproc
import org.bytedeco.opencv.global.opencv_imgproc._
import org.bytedeco.opencv.opencv_core._
import org.rogach.scallop._
import org.slf4j.LoggerFactory

import java.awt.Color
import java.io.File
import java.nio.file.{Path, Paths}

case class Deskewer(outDir: Option[Path] = None, debugDir: Option[Path] = None)
    extends ImageTransformer[SkewAngle]
    with ImageUtils {
  private val log = LoggerFactory.getLogger(getClass)
  private val config = ConfigFactory.load().getConfig("jochre.ocr.deskewer")
  private val maxContours = config.getInt("max-contours-for-calculation")

  override def transform(path: String, mat: Mat): (Mat, SkewAngle) = {
    val angle = this.getSkewAngle(mat, Some(path))

    deskew(path, mat, angle) -> SkewAngle(angle.getOrElse(0.0))
  }

  def deskew(path: String, mat: Mat, angle: Option[Double]): Mat = {
    val rotated = angle.map(this.unrotate(_, mat)).getOrElse(mat)

    val baseName = FileUtils.removeFileExtension(new File(path).getName)
    outDir.foreach(outDir => saveImage(rotated, outDir.resolve(f"${baseName}_deskewered.jpg")))
    rotated
  }

  def getSkewAngle(mat: Mat, path: Option[String] = None): Option[Double] = {
    log.debug(f"Deskewing $path")

    val baseName = path
      .map(path => FileUtils.removeFileExtension(new File(path).getName))
      .getOrElse("test")

    val resizer = new ResizeImageAndKeepAspectRatio(1000, 1000)
    val (resized, _) = resizer.transform(baseName, mat)

    val colored = toRGB(resized)

    // Image preparation: blur, and threshold
    val blur = new Mat()
    GaussianBlur(resized, blur, new Size(9, 9), 0)

    debugDir.foreach(debugDir => saveImage(blur, debugDir.resolve(f"${baseName}_deskewer1_blur.jpg")))

    val thresh = new Mat()
    threshold(blur, thresh, 0, 255, THRESH_BINARY_INV + THRESH_OTSU)

    debugDir.foreach(debugDir =>
      saveImage(
        thresh,
        debugDir.resolve(f"${baseName}__deskewer2_threshold.jpg")
      )
    )

    // thresh = cv2.threshold (blur, 0, 255, cv2.THRESH_BINARY_INV + cv2.THRESH_OTSU)[1]

    // Apply dilate to merge text into meaningful lines / paragraphs.
    // Use larger kernel on X axis to merge characters into single line, cancelling out any spaces.
    // But use smaller kernel on Y axis to separate between different blocks of text
    val kernel = getStructuringElement(MORPH_RECT, new Size(10, 3))
    val dilated = new Mat()
    dilate(
      thresh,
      dilated,
      kernel,
      new Point(-1, -1),
      3,
      BORDER_CONSTANT,
      new Scalar(morphologyDefaultBorderValue)
    )

    debugDir.foreach(debugDir =>
      saveImage(
        dilated,
        Paths.get(debugDir.toString, baseName + "_deskewer3_dilated.jpg")
      )
    )

    // Find all contours
    val mode = opencv_imgproc.RETR_LIST
    val method = opencv_imgproc.CHAIN_APPROX_SIMPLE
    val contours: MatVector = new MatVector

    findContours(dilated, contours, mode, method)

    val contoursByDecreasingArea = (0 until contours.size().toInt)
      .map { i =>
        val contour: Mat = contours.get(i)
        val area = contourArea(contour)
        (contour, area)
      }
      .sortBy(0 - _._2)

    val effectiveMaxContours = math.min(
      maxContours,
      math.ceil(contoursByDecreasingArea.size.toDouble / 2).toInt
    )
    log.debug(
      f"Calculating based on $effectiveMaxContours contours (found ${contoursByDecreasingArea.size} contours)"
    )

    // in case the max contour is an outlier (e.g. an image without borders), we take the top n contours
    val contoursForCalculation =
      contoursByDecreasingArea.take(effectiveMaxContours)

    val contoursWithRectangles = contoursForCalculation.flatMap { case (contour, area) =>
      try {
        val rotatedRect: RotatedRect = minAreaRect(contour)

        drawRotatedRect(colored, rotatedRect, Color.green, thickness = 5)

        Some(contour, area, rotatedRect, Rectangle(rotatedRect))
      } catch {
        case e: Throwable =>
          // Avoid rare RuntimeException on minAreaRect
          log.error("Unable to find minAreaRect for contour", e)
          None
      }
    }

    val noContains = contoursWithRectangles.zipWithIndex
      .filterNot { case ((_, _, _, smallerRect), i) =>
        contoursWithRectangles
          .slice(0, i)
          .exists { case (_, _, _, biggerRect) =>
            val overlap = biggerRect.intersection(smallerRect)
            val overlapPercentage = overlap
              .map(_.area.toDouble / smallerRect.area.toDouble)
              .getOrElse(0.0)
            val contains = overlapPercentage > 0.5

            if (contains) {
              log.debug(f"Found $biggerRect containing $smallerRect")
              log.debug(f"Overlap: $overlap")
              log.debug(f"Overlap %%: $overlapPercentage")
            }
            contains
          }
      }
      .map(_._1)

    case class ContourWithAngle(
        contour: Mat,
        area: Double,
        rotatedRect: RotatedRect,
        container: Rectangle,
        correctedAngle: Double
    )
    val contoursWithAngles = noContains.zipWithIndex
      .map { case ((contour, area, rotatedRect, container), i) =>
        log.debug(f"Contour $i with area $area")

        val angle = -1.0 * BigDecimal(rotatedRect.angle())
          .setScale(2, BigDecimal.RoundingMode.HALF_UP)
          .toDouble

        // Convert the angle to a value usable for image skewing
        val correctedAngle = if (angle < -45) {
          90 + angle
        } else if (angle > 45) {
          90 - angle
        } else {
          angle
        }

        log.debug(f"angle: $correctedAngle")
        ContourWithAngle(contour, area, rotatedRect, container, correctedAngle)
      }
      .sortBy(_.correctedAngle)

    Option.when(contoursWithAngles.nonEmpty) {
      // remove outliers
      val medianAngle =
        contoursWithAngles(contoursWithAngles.size / 2).correctedAngle
      log.debug(f"medianAngle: $medianAngle")

      val inliers = contoursWithAngles.filter { case ContourWithAngle(_, _, rotatedRect, container, angle) =>
        val isInlier =
          medianAngle - 1.0 <= angle && angle <= medianAngle + 1.0
        if (isInlier) {
          log.debug(
            f"Found inlier with angle $angle (median angle: $medianAngle): $container"
          )
          drawRotatedRect(colored, rotatedRect, Color.red, thickness = 5)
        } else {
          log.debug(
            f"Found outlier with angle $angle (median angle: $medianAngle): $container"
          )
          drawRotatedRect(colored, rotatedRect, Color.blue, thickness = 5)
        }
        isInlier
      }

      debugDir.foreach(debugDir =>
        saveImage(
          colored,
          debugDir.resolve(f"${baseName}__deskewer4_rectangle.jpg")
        )
      )

      val meanAngle = inliers.map(_.correctedAngle).sum / inliers.size
      log.info(f"Deskewed $path, meanAngle: $meanAngle%.3f")
      meanAngle
    }
  }
}

object Deskewer extends ImageUtils {
  private val log = LoggerFactory.getLogger(getClass)

  private class DeskewerCLI(arguments: Seq[String]) extends ScallopConf(arguments) {
    val inputDir: ScallopOption[String] = opt[String](required = true)
    val outDir: ScallopOption[String] = opt[String](required = true)
    val debugDir: ScallopOption[String] = opt[String](required = false)
    val longSide: ScallopOption[Int] = opt[Int](required = false)
    verify()
  }

  def main(args: Array[String]): Unit = {
    val options = new DeskewerCLI(args.toIndexedSeq)

    val inputDir = Path.of(options.inputDir())
    val outDir = Path.of(options.outDir())
    outDir.toFile.mkdirs()
    val debugDir = options.debugDir.toOption.map(Path.of(_))
    debugDir.foreach(_.toFile.mkdirs())

    val longSide = options.longSide.toOption

    val files = FileUtils.listImages(inputDir)

    val deskewer = Deskewer(Some(outDir), debugDir)

    val transforms = List[Option[ImageTransformer[?]]](
      longSide.map(longSide => new ResizeImageAndKeepAspectRatio(longSide, longSide))
    ).flatten

    var diffs = Seq.empty[Double]
    var golds = Seq.empty[Double]

    files.map { file =>
      log.debug(f"Processing ${file.getPath}")
      val mat = loadImage(file.toPath)

      val transformed: Mat = transforms.foldLeft(mat) { case (mat, transformer) =>
        transformer.transform(file.getPath, mat)._1
      }

      val calculated = deskewer.getSkewAngle(transformed, Some(file.getPath))
      val altoFinder = AltoFinder.default
      val alto = altoFinder.getAlto(file.toPath)
      val expected = alto.pages.head.rotation
      val calculatedOrZero = calculated.getOrElse(0.0)

      log.info(f"Calculated: $calculatedOrZero")
      log.info(f"Expected:   $expected")

      diffs = diffs :+ Math.abs(calculatedOrZero - expected)
      golds = golds :+ expected

      deskewer.deskew(file.getPath, mat, calculated)
    }

    val mean = MathUtils.mean(diffs)
    val stdDev = MathUtils.stdDev(diffs)
    log.info(f"Count: ${diffs.size}. Mean: $mean%2.3f. Std dev $stdDev%2.3f")

    log.info(
      f"Gold count: ${golds.size}. Mean: ${MathUtils.mean(golds)}%2.3f. Std edv ${MathUtils.stdDev(golds)}%2.3f."
    )
  }
}
