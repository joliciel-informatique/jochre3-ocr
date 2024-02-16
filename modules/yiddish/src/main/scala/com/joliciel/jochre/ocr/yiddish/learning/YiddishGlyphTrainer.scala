package com.joliciel.jochre.ocr.yiddish.learning

import com.joliciel.jochre.ocr.core.learning.{GlyphTrainer, ModelBuilder}
import com.joliciel.jochre.ocr.yiddish.YiddishTextSimpifier
import enumeratum._
import org.rogach.scallop.{ScallopConf, ScallopOption}

import java.nio.file.Path

object YiddishGlyphTrainer {
  class TrainerCLI(arguments: Seq[String]) extends ScallopConf(arguments) {
    val corpusDir: ScallopOption[String] = opt[String](required = true)
    val outputDir: ScallopOption[String] = opt[String](required = true)
    val modelName: ScallopOption[String] = opt[String](required = true)
    val numEpochs: ScallopOption[Int] = opt[Int](default = Some(20), descr = "Number of training epochs, default = 20.")
    val batchSize: ScallopOption[Int] = opt[Int](default = Some(32), descr = "Batch size, default = 32.")
    val modelType: ScallopOption[String] = opt[String](required = true, descr = f"Model type, among ${ModelType.values.map(_.entryName).mkString(", ")}")

    verify()
  }

  val textSimplifier = YiddishTextSimpifier(true)

  def main(args: Array[String]): Unit = {
    val cli = new TrainerCLI(args)
    val corpusDir = Path.of(cli.corpusDir())
    val outputDir = Path.of(cli.outputDir())
    val modelName = cli.modelName()
    val numEpochs = cli.numEpochs()
    val batchSize = cli.batchSize()
    val modelType = ModelType.withName(cli.modelType())

    val glyphModel = modelType.glyphTrainerModelType

    outputDir.toFile.mkdirs()

    val trainer = GlyphTrainer(corpusDir, outputDir,
      modelType = glyphModel,
      modelName = modelName,
      numEpochs = numEpochs,
      batchSize = batchSize,
      textSimplifier = textSimplifier
    )
    trainer.train()
  }

  sealed trait ModelType extends EnumEntry {
    def glyphTrainerModelType: ModelBuilder.ModelType
  }

  object ModelType extends Enum[ModelType] {
    val values = findValues

    case object CNN extends ModelType {
      override def glyphTrainerModelType: ModelBuilder.ModelType = ModelBuilder.ModelType.CNNModelWithMaxPooling()
    }

    case object CNNStride extends ModelType {
      override def glyphTrainerModelType: ModelBuilder.ModelType = ModelBuilder.ModelType.CNNModelWithStride()
    }

    case object MLP extends ModelType {
      override def glyphTrainerModelType: ModelBuilder.ModelType = ModelBuilder.ModelType.MLPModel()
    }
  }
}
