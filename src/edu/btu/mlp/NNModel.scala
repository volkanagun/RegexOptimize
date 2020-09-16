package edu.btu.mlp

import org.deeplearning4j.nn.conf.{ComputationGraphConfiguration, NeuralNetConfiguration, WorkspaceMode}
import org.deeplearning4j.nn.conf.graph.MergeVertex
import org.deeplearning4j.nn.conf.layers.ConvolutionLayer.AlgoMode
import org.deeplearning4j.nn.conf.layers.recurrent.LastTimeStep
import org.deeplearning4j.nn.conf.layers.{DenseLayer, EmbeddingSequenceLayer, LSTM, OutputLayer, RnnOutputLayer}
import org.deeplearning4j.nn.conf.preprocessor.{FeedForwardToRnnPreProcessor, RnnToFeedForwardPreProcessor}
import org.nd4j.linalg.activations.Activation
import org.nd4j.linalg.learning.config.Adam
import org.nd4j.linalg.lossfunctions.LossFunctions.LossFunction

class NNModel {

  var windowLength:Int = 0;
  var embeddingLength:Int = 0;
  var hiddenLength:Int = 0;
  var maxVocabLength:Int = 256;
  var maxCharLength:Int = 256;
  var lrate:Double = 0.005;

  def autoGenerativeLSTM(): ComputationGraphConfiguration = {

      val conf = new NeuralNetConfiguration.Builder()
        .cudnnAlgoMode(AlgoMode.NO_WORKSPACE)
        .updater(new Adam.Builder().learningRate(lrate).build())
        .dropOut(0.5)
        .graphBuilder()
        .allowDisconnected(true)
        .addInputs("input")
        /*.addVertex("stack", new org.deeplearning4j.nn.conf.graph.StackVertex(), "left", "right")
        */.addLayer("emb", new EmbeddingSequenceLayer.Builder().inputLength(windowLength)
          .nIn(maxCharLength).nOut(embeddingLength).build(),
          "input")
        /*.addVertex("left", new org.deeplearning4j.nn.conf.graph.UnstackVertex(0, 2), "emb")
        .addVertex("right", new org.deeplearning4j.nn.conf.graph.UnstackVertex(0, 2), "emb")
        .addLayer("leftout", new LastTimeStep(new LSTM.Builder().nIn(embeddingLength).nOut(hiddenLength)
          .activation(Activation.RELU)
          .build()), "left")
        */.addLayer("hidden", new LastTimeStep(new LSTM.Builder().nIn(embeddingLength).nOut(hiddenLength)
          .activation(Activation.RELU)
          .build()), "emb")
        //.addVertex("merge", new MergeVertex(), "leftout", "rightout")
        .addLayer("mlpIn", new DenseLayer.Builder().nIn(hiddenLength).nOut(embeddingLength)
          .activation(Activation.RELU)
          .build(), "hidden")
        .addLayer("mlpOut", new DenseLayer.Builder().nIn(embeddingLength).nOut(hiddenLength)
          .activation(Activation.RELU).build(), "mlpIn")
        .addLayer("outputActivation", new RnnOutputLayer.Builder(LossFunction.NEGATIVELOGLIKELIHOOD)
          .activation(Activation.SOFTMAX).nIn(hiddenLength)
          .nOut(maxVocabLength).build(), "mlpOut")
        .setOutputs("outputActivation")
        .inputPreProcessor("outputActivation", new FeedForwardToRnnPreProcessor())
        .build();

      conf.setTrainingWorkspaceMode(WorkspaceMode.NONE)
      conf.setInferenceWorkspaceMode(WorkspaceMode.NONE)
      conf
  }

  def autoGenerativeStepLSTM():Unit = {

  }





}
