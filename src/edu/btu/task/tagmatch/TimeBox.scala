package edu.btu.task.tagmatch

object TimeBox {

  var countMap = Map[String, Int]()
  var timeMap = Map[String, Long]()


  def measureTime[T](name:String, f: =>T): T ={
    val start = System.nanoTime()
    val result = f
    val end = System.nanoTime()
    append(name, end-start)
    result
  }

  def append(name:String, time:Long):this.type ={
    countMap = countMap.updated(name, countMap.getOrElse(name, 0)+1)
    timeMap = timeMap.updated(name, timeMap.getOrElse(name, 0L)+time)
    this
  }

  def summary(): Unit ={
    timeMap.foreach{case(name, time)=>{
      println(s"Avg. time taken for name: ${name} is ${time / (1000 * 1000 * countMap(name)) } ms")
    }}
  }
}

