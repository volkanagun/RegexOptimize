package edu.btu.task.tagmatch

object Main {

  def main(args: Array[String]): Unit = {
    new TagExperiment()
      .evaluate()
      .summary()
  }
}
