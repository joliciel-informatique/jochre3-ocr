package com.joliciel.jochre.ocr.core.learning

import ai.djl.Model
import ai.djl.engine.Engine
import ai.djl.metric.Metrics
import ai.djl.ndarray.types.Shape
import ai.djl.training.evaluator.Accuracy
import ai.djl.training.listener.{SaveModelTrainingListener, TrainingListener}
import ai.djl.training.loss.Loss
import ai.djl.training.optimizer.Adam
import ai.djl.training.tracker.Tracker
import ai.djl.training.util.ProgressBar
import ai.djl.training.{DefaultTrainingConfig, EasyTrain, TrainingResult}
import com.joliciel.jochre.ocr.core.corpus.{AltoFinder, TextSimplifier}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.util.matching.Regex

/** @param checkpoint
  *   save the model at the end of the epoch every checkpoint, default is 1
  * @param wordSelectionRegex
  *   if provided, will limit this trainer to glyphs contained in words whose content matches the regex
  */
case class GlyphTrainer(
    corpusDir: Path,
    outputDir: Path,
    modelName: String,
    numEpochs: Int = 10,
    modelType: ModelBuilder.ModelType,
    imageSize: Int = 28,
    batchSize: Int = 32,
    limit: Int = 0,
    maxGpus: Int = 0,
    checkpoint: Int = 1,
    wordSelectionRegex: Option[Regex] = None,
    textSimplifier: TextSimplifier = TextSimplifier.default,
    altoFinder: AltoFinder = AltoFinder.default
) {
  def train(): TrainingResult = {
    val model = Model.newInstance(modelName)

    val classesPath = outputDir.resolve(f"${modelName}_classes.txt")
    Files.write(
      classesPath,
      dataset.classes.mkString("\n").getBytes(StandardCharsets.UTF_8)
    )

    val trainingResult =
      try {
        model.setBlock(modelType.getModel(dataset.classes.size, imageSize))
        // get training and validation dataset
        val splits = dataset.randomSplit(9, 1)
        val trainingSet = splits(0)
        val validateSet = splits(1)
        // setup training configuration
        val trainer = model.newTrainer(trainingConfig)
        val trainingResult =
          try {
            trainer.setMetrics(new Metrics())
            val inputShape = new Shape(batchSize, 1, imageSize, imageSize)
            // initialize trainer with proper input shape
            trainer.initialize(inputShape)
            EasyTrain.fit(trainer, numEpochs, trainingSet, validateSet)
            trainer.getTrainingResult
          } finally trainer.close()
        trainingResult
      } finally model.close()

    trainingResult
  }

  private val trainingConfig: DefaultTrainingConfig = {
    val listener = new SaveModelTrainingListener(
      outputDir.toFile.getPath,
      modelName,
      checkpoint
    )
    listener.setSaveModelCallback(trainer => {
      val result = trainer.getTrainingResult
      val model = trainer.getModel
      val accuracy = result.getValidateEvaluation("Accuracy")
      model.setProperty("Accuracy", String.format("%.5f", accuracy))
      model.setProperty("Loss", String.format("%.5f", result.getValidateLoss))
    })

    val learningRateTracker = Tracker
      .cosine()
      .setBaseValue(1e-3f)
      .optFinalValue(1e-4f)
      .setMaxUpdates(20)
      .build()

    val optimizer =
      Adam
        .builder()
        .optEpsilon(1e-5f)
        .optLearningRateTracker(learningRateTracker)
        .build();

    new DefaultTrainingConfig(Loss.softmaxCrossEntropyLoss)
      .addEvaluator(new Accuracy())
      .optDevices(Engine.getInstance.getDevices(maxGpus))
      .optOptimizer(optimizer)
      .addTrainingListeners(
        TrainingListener.Defaults.logging(outputDir.toFile.getPath): _*
      )
      .addTrainingListeners(listener)
  }

  val dataset: GlyphDataset = {
    val dataset = GlyphDatasetBuilder(
      corpusDir,
      imageSize,
      textSimplifier = textSimplifier,
      altoFinder = altoFinder,
      wordSelectionRegex = wordSelectionRegex
    ).setSampling(batchSize, false, true).build()
    dataset.prepare(new ProgressBar())
    dataset
  }
}
