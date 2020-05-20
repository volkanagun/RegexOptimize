package edu.btu.task.tagmatch

class TagExperiment {
 //measure efficiency of tag generation
 //measure efficiency of filtering
 //measure accuracy

  //cross-validation
  //training vs test vs all
  var trainTestRatio = Array(0.6, 0.4)

  var totalSampleCount = 0
  var totalDomainCount = 0
  var totalFileCount = 0

  //number of samples per domain
  var domainSampleMap = Map[String, List[TagSample]]()
  var domainFileCount = Map[String, Int]()
  var fileSampleMap = Map[String, List[TagSample]]



  //do the train/test for each domain
  //do the accuracy for each domain



}
