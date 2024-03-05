package com.joliciel.jochre.ocr.core.learning

import ai.djl.basicmodelzoo.basic.Mlp
import ai.djl.ndarray.NDList
import ai.djl.ndarray.types.Shape
import ai.djl.nn.convolutional.Conv2d
import ai.djl.nn.core.Linear
import ai.djl.nn.norm.{BatchNorm, Dropout}
import ai.djl.nn.pooling.Pool
import ai.djl.nn.{Activation, Block, Blocks, SequentialBlock}

case class ModelBuilder(numClasses: Int, dropOut: Float = 0.4f, denseLayer: Int = 128) {
  private def dropOutLayer: Dropout = Dropout.builder().optRate(dropOut).build()
  private def batchNormLayer: BatchNorm = BatchNorm.builder().build()

  private def convLayer(filters: Int, kernel: Int, stride: Int = 1, padding: Int = 2): Conv2d =
    Conv2d.builder()
      .setKernelShape(new Shape(kernel, kernel))
      .optPadding(new Shape(padding, padding))
      .optStride(new Shape(stride, stride))
      .optBias(false)
      .setFilters(filters)
      .build()

  private def maxPoolLayer(kernel: Int, stride: Int = 2, padding: Int = 2): Block =
    Pool.maxPool2dBlock(new Shape(5, 5), new Shape(2, 2), new Shape(2, 2))


  private val modelWithMaxPooling: Block = {
    val block = new SequentialBlock()

    block
      .add(convLayer(32, 5))
      .add { (list: NDList) => Activation.relu(list) }
      .add(maxPoolLayer(5))
      .add(dropOutLayer)
      .add(convLayer(64, 5))
      .add{ (list: NDList) => Activation.relu(list) }
      .add(maxPoolLayer(5))
      .add(dropOutLayer)
      // Blocks.batchFlattenBlock() will transform the input of the shape (batch size, channel,
      // height, width) into the input of the shape (batch size,
      // channel * height * width)
      .add(Blocks.batchFlattenBlock())
      .add(Linear.builder().setUnits(denseLayer).build())
      .add { (list: NDList) => Activation.relu(list) }
      .add(dropOutLayer)
      .add(Linear.builder().setUnits(numClasses).build());
  }

  private val modelWithStride: Block = {
    val block = new SequentialBlock()

    block
      .add(convLayer(32, 3))
      .add { (list: NDList) => Activation.relu(list) }
      .add(batchNormLayer)
      .add(convLayer(32, 3))
      .add { (list: NDList) => Activation.relu(list) }
      .add(batchNormLayer)
      .add(convLayer(32, 5, stride = 2))
      .add { (list: NDList) => Activation.relu(list) }
      .add(batchNormLayer)
      .add(dropOutLayer)
      .add(convLayer(64, 3))
      .add { (list: NDList) => Activation.relu(list) }
      .add(batchNormLayer)
      .add(convLayer(64, 3))
      .add { (list: NDList) => Activation.relu(list) }
      .add(batchNormLayer)
      .add(convLayer(64, 5, stride = 2))
      .add { (list: NDList) => Activation.relu(list) }
      .add(batchNormLayer)
      .add(dropOutLayer)
      // Blocks.batchFlattenBlock() will transform the input of the shape (batch size, channel,
      // height, width) into the input of the shape (batch size,
      // channel * height * width)
      .add(Blocks.batchFlattenBlock())
      .add(Linear.builder().setUnits(denseLayer).build())
      .add { (list: NDList) => Activation.relu(list) }
      .add(BatchNorm.builder().build())
      .add(dropOutLayer)
      .add(Linear.builder().setUnits(numClasses).build());
  }
}

object ModelBuilder {
  sealed trait ModelType {
    def getModel(numClasses: Int, imageSize: Int = 28): Block
  }

  object ModelType {
    case class CNNModelWithMaxPooling(dropOut: Float = 0.4f, denseLayer: Int = 128) extends ModelType {
      override def getModel(numClasses: Int, imageSize: Int): Block = ModelBuilder(numClasses, dropOut, denseLayer).modelWithMaxPooling
    }

    case class CNNModelWithStride(dropOut: Float = 0.4f, denseLayer: Int = 128) extends ModelType {
      override def getModel(numClasses: Int, imageSize: Int): Block = ModelBuilder(numClasses, dropOut, denseLayer).modelWithStride
    }

    case class MLPModel(layers: Array[Int] = Array[Int](128, 64)) extends ModelType {
      override def getModel(numClasses: Int, imageSize: Int): Block = new Mlp(imageSize * imageSize, numClasses, layers)
    }
  }
}
