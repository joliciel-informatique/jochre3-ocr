package com.joliciel.jochre.ocr.yiddish.learning

import com.joliciel.jochre.ocr.core.learning.GlyphEvaluator
import com.joliciel.jochre.ocr.yiddish.YiddishTextSimpifier
import com.joliciel.jochre.ocr.yiddish.learning.YiddishGlyphTrainer.ModelType
import org.rogach.scallop._
import org.slf4j.LoggerFactory

import java.nio.file.Path
import scala.collection.compat.immutable.ArraySeq

object YiddishGlyphEvaluator {
  private val log = LoggerFactory.getLogger(getClass)

  class TrainerCLI(arguments: Seq[String]) extends ScallopConf(arguments) {
    val corpusDir: ScallopOption[String] = opt[String](required = true)
    val outputDir: ScallopOption[String] = opt[String](required = true)
    val modelDir: ScallopOption[String] = opt[String](required = true)
    val modelName: ScallopOption[String] = opt[String](required = true)
    val modelType: ScallopOption[String] = opt[String](
      required = true,
      descr = f"Model type, among ${ModelType.values.map(_.entryName).mkString(", ")}"
    )
    verify()
  }

  val textSimplifier: YiddishTextSimpifier = YiddishTextSimpifier(true)

  def main(args: Array[String]): Unit = {
    val cli = new TrainerCLI(ArraySeq.unsafeWrapArray(args))
    val corpusDir = Path.of(cli.corpusDir())
    val outputDir = Path.of(cli.outputDir())
    val modelDir = Path.of(cli.modelDir())
    val modelName = cli.modelName()
    val modelType = ModelType.withName(cli.modelType())

    val glyphModel = modelType.glyphTrainerModelType

    val evaluator = GlyphEvaluator(
      corpusDir,
      outputDir,
      modelDir,
      modelName,
      modelType = glyphModel,
      textSimplifier = textSimplifier
    )
    val accuracy = evaluator.evaluate()
    log.info(f"Accuracy: $accuracy%.5f")
  }
}
