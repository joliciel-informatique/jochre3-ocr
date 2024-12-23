package com.joliciel.jochre.ocr.yiddish.learning

import com.joliciel.jochre.ocr.core.corpus.TextSimplifier
import com.joliciel.jochre.ocr.core.learning.{GlyphTrainer, ModelBuilder}
import com.joliciel.jochre.ocr.yiddish.YiddishTextSimpifier
import enumeratum._
import org.rogach.scallop._

import java.nio.file.Path
import scala.collection.compat.immutable.ArraySeq
import scala.util.matching.Regex

object YiddishGlyphTrainer {
  private class TrainerCLI(arguments: Seq[String]) extends ScallopConf(arguments) {
    val corpusDir: ScallopOption[String] = opt[String](required = true)
    val outputDir: ScallopOption[String] = opt[String](required = true)
    val modelName: ScallopOption[String] = opt[String](required = true)
    val numEpochs: ScallopOption[Int] = opt[Int](
      default = Some(10),
      descr = "Number of training epochs, default = 10."
    )
    val batchSize: ScallopOption[Int] =
      opt[Int](default = Some(32), descr = "Batch size, default = 32.")
    val modelType: ScallopOption[String] = opt[String](
      required = true,
      descr = f"Model type, among ${ModelType.values.map(_.entryName).mkString(", ")}"
    )
    val alphabet: ScallopOption[String] = opt[String](
      default = Some(Alphabet.Hebrew.entryName),
      descr = f"Alphabet, among ${Alphabet.values.map(_.entryName).mkString(", ")}"
    )
    verify()
  }

  def main(args: Array[String]): Unit = {
    val cli = new TrainerCLI(ArraySeq.unsafeWrapArray(args))
    val corpusDir = Path.of(cli.corpusDir())
    val outputDir = Path.of(cli.outputDir())
    val modelName = cli.modelName()
    val numEpochs = cli.numEpochs()
    val batchSize = cli.batchSize()
    val modelType = ModelType.withName(cli.modelType())
    val alphabet = Alphabet.withName(cli.alphabet())

    val glyphModel = modelType.glyphTrainerModelType

    outputDir.toFile.mkdirs()

    val trainer = GlyphTrainer(
      corpusDir,
      outputDir,
      modelType = glyphModel,
      modelName = modelName,
      numEpochs = numEpochs,
      batchSize = batchSize,
      textSimplifier = alphabet.textSimplifier,
      wordSelectionRegex = alphabet.regex
    )
    trainer.train()
  }

  sealed trait ModelType extends EnumEntry {
    def glyphTrainerModelType: ModelBuilder.ModelType
  }

  object ModelType extends Enum[ModelType] {
    val values: IndexedSeq[ModelType] = findValues

    case object CNN extends ModelType {
      override def glyphTrainerModelType: ModelBuilder.ModelType =
        ModelBuilder.ModelType.CNNModelWithMaxPooling()
    }

    case object CNNStride extends ModelType {
      override def glyphTrainerModelType: ModelBuilder.ModelType =
        ModelBuilder.ModelType.CNNModelWithStride()
    }

    case object MLP extends ModelType {
      override def glyphTrainerModelType: ModelBuilder.ModelType =
        ModelBuilder.ModelType.MLPModel()
    }
  }

  private sealed trait Alphabet extends EnumEntry {
    def regex: Option[Regex]
    def textSimplifier: TextSimplifier = TextSimplifier.default
  }

  private object Alphabet extends Enum[Alphabet] {
    val values: IndexedSeq[Alphabet] = findValues

    case object Hebrew extends Alphabet {
      val regex: Option[Regex] = None
      override val textSimplifier: YiddishTextSimpifier =
        YiddishTextSimpifier(replaceNonHebrewAlphabets = true)
    }

    case object Latin extends Alphabet {
      val regex: Option[Regex] = Some(raw""".*\p{IsLatin}.*""".r)
    }
    case object Cyrillic extends Alphabet {
      val regex: Option[Regex] = Some(raw""".*\p{IsCyrillic}.*""".r)
    }
  }
}
