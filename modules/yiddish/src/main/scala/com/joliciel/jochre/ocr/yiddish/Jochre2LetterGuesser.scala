package com.joliciel.jochre.ocr.yiddish

import com.joliciel.jochre.analyser.{BeamSearchImageAnalyser, LetterAssigner, LetterGuessObserver}
import com.joliciel.jochre.boundaries.{BoundaryDetector, OriginalBoundaryDetector}
import com.joliciel.jochre.graphics.{ImageStatus, RowOfShapes, Shape, SourceImage}
import com.joliciel.jochre.letterGuesser.LetterGuesser
import com.joliciel.jochre.letterGuesser.features.{LetterFeature, LetterFeatureParser}
import com.joliciel.jochre.lexicon.MostLikelyWordChooser
import com.joliciel.jochre.ocr.core.model.{Glyph, Page, TextBlock, TextLine, Word}
import com.joliciel.jochre.ocr.core.text.{TextGuesser, TextGuesserService}
import com.joliciel.jochre.ocr.core.utils.{ImageUtils, OutputLocation}
import com.joliciel.jochre.yiddish.JochreYiddish
import com.joliciel.talismane.machineLearning.ClassificationModel
import com.typesafe.config.ConfigFactory
import org.bytedeco.opencv.opencv_core.Mat
import org.slf4j.LoggerFactory
import zio.{Task, ZIO, ZLayer}

import java.util
import scala.collection.mutable
import scala.jdk.CollectionConverters._

object Jochre2LetterGuesserService {
  val live: ZLayer[Any, Nothing, TextGuesserService] = ZLayer.succeed(Jochre2LetterGuesserServiceImpl)
}

private object Jochre2LetterGuesserServiceImpl extends TextGuesserService {
  def getTextGuesser(): Task[Jochre2LetterGuesser] = {
    ZIO.attempt(Jochre2LetterGuesser())
  }
}

case class Jochre2LetterGuesser() extends TextGuesser with ImageUtils {
  private val log = LoggerFactory.getLogger(getClass)

  private val yiddishConfig = ConfigFactory.load().getConfig("jochre.ocr.yiddish")
  private val lexiconPath = yiddishConfig.getString("lexicon-path")
  private val letterModelPath = yiddishConfig.getString("letter-model-path")

  private val jochreConfig = ConfigFactory.load()
  private val args = mutable.Map(
    "lexicon" -> lexiconPath,
    "letterModel" -> letterModelPath,
    "isCleanSegment" -> "true"
  )
  private val jochreYiddish: JochreYiddish = new JochreYiddish(jochreConfig, args.asJava)

  /**
   * Given an image and a pre-segmented [[Page]] structure, attempt to guess the text within the page
   * by assigning content to the resulting page.
   */
  override def guess(page: Page, mat: Mat, fileName: String, debugLocation: Option[OutputLocation]): Task[Page] = ZIO.attempt{
    val jochreSession = jochreYiddish.getJochreSession
    val originalImage = toBufferedImage(mat)
    val jochreImage = new SourceImage(fileName, originalImage, jochreSession)
    jochreImage.setImageStatus(ImageStatus.AUTO_NEW);
    jochreImage.setWidth(mat.cols())
    jochreImage.setHeight(mat.rows())

    val wordsGroupsAndGlyphToShape = page.textBlocks.flatMap { textBlock =>
      val paragraph = jochreImage.newParagraph()
      textBlock.textLines.flatMap { textLine =>
        val row = new RowOfShapes(jochreImage, jochreSession)
        jochreImage.addRow(row)
        val tuple = textLine.words.map { word =>
          val glyphToShape = word.glyphs.map { glyph =>
            val baseLeft = glyph.rectangle.left
            val baseTop = glyph.rectangle.top
            val initialShape = new Shape(jochreImage, baseLeft, baseTop, glyph.rectangle.right-1, glyph.rectangle.bottom-1, jochreSession)
            // need to calculate actual shape left/top/right/bottom
            val left = (0 until initialShape.getWidth).find(x => (0 until initialShape.getHeight).find(y => initialShape.isPixelBlack(x,y)).isDefined).getOrElse(-1)
            val top = (0 until initialShape.getHeight).find(y => (0 until initialShape.getWidth).find(x => initialShape.isPixelBlack(x,y)).isDefined).getOrElse(-1)
            val right = (0 until initialShape.getWidth).reverse.find(x => (0 until initialShape.getHeight).find(y => initialShape.isPixelBlack(x,y)).isDefined).getOrElse(-1)
            val bottom = (0 until initialShape.getHeight).reverse.find(y => (0 until initialShape.getWidth).find(x => initialShape.isPixelBlack(x,y)).isDefined).getOrElse(-1)

            val shape = Option.when(right>=0)(new Shape(jochreImage, baseLeft+left, baseTop+top, baseLeft+right, baseTop+bottom, jochreSession))
            glyph -> shape
          }

          val hasShapes = glyphToShape.find(_._2.isDefined).isDefined
          val group = Option.when(hasShapes){
            val group = row.newGroup()
            glyphToShape.flatMap(_._2).foreach { shape =>
              group.addShape(shape)
              row.addShape(shape)
            }
            group
          }
          (word, group, glyphToShape)
        }
        if (row.getGroups.size()==0) {
          jochreImage.removeRow(row)
        } else {
          paragraph.addRow(row)
        }
        tuple
      }
    }

    val wordToGroupMap = wordsGroupsAndGlyphToShape.map{
      case (word, group, _) => word -> group
    }.toMap

    val glyphToShapeMap = wordsGroupsAndGlyphToShape.flatMap{
      case (_, _, glyphToShape) => glyphToShape
    }.toMap

    val paragraphs = jochreImage.getParagraphs.asScala.filter(_.getRows.size()>0)
    jochreImage.clearSegmentation()
    paragraphs.foreach(jochreImage.getParagraphs.add(_))
    jochreImage.getRows.forEach(_.assignGuideLines())
    jochreImage.restoreOriginalImage()
    jochreImage.recalculateIndexes()
    jochreImage.setShapeCount(glyphToShapeMap.size)

    val wordChooser = new MostLikelyWordChooser(jochreSession)

    val letterModel: ClassificationModel = jochreSession.getLetterModel

    val letterFeatureDescriptors: util.List[String] = letterModel.getFeatureDescriptors
    val letterFeatureParser: LetterFeatureParser = new LetterFeatureParser
    val letterFeatures: util.Set[LetterFeature[_]] = letterFeatureParser.getLetterFeatureSet(letterFeatureDescriptors)
    val letterGuesser: LetterGuesser = new LetterGuesser(letterFeatures, letterModel.getDecisionMaker)

    val boundaryDetector: BoundaryDetector = new OriginalBoundaryDetector
    val letterGuessObserver: LetterGuessObserver = new LetterAssigner
    val analyser = new BeamSearchImageAnalyser(boundaryDetector, letterGuesser, wordChooser, jochreSession)
    analyser.addObserver(letterGuessObserver)

    analyser.analyse(jochreImage)

    val analysedPage = page.copy(blocks = page.blocks.map{
      case TextBlock(rectangle, textLines) =>
        TextBlock(rectangle, textLines.map{
          case textLine@TextLine(_, wordsAndSpaces) =>
          textLine.copy(wordsAndSpaces = wordsAndSpaces.flatMap{
            case word@Word(rectangle, glyphs, _) =>
              val newGlyphs = glyphs.flatMap{
                case glyph@Glyph(rectangle, _) =>
                  glyphToShapeMap(glyph) match {
                    case Some(shape) => Some(glyph.copy(rectangle = rectangle.copy(label = shape.getLetter), confidence = shape.getConfidence))
                    case None => None
                  }
              }
              wordToGroupMap(word) match {
                case Some(group) =>
                  val wordContent = group.getWord
                  Some(word.copy(rectangle = rectangle.copy(label = wordContent), glyphs = newGlyphs, confidence = group.getConfidence))
                case None => None
              }

            case other => Some(other)
          })
        })
      case other => other
    })

    analysedPage
  }
}
