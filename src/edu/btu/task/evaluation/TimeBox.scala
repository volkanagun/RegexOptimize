package edu.btu.task.evaluation

object TimeBox {

  var countMap = Map[String, Long]()
  var timeMap = Map[String, Long]()


  def measureTime[T](name:String, f: =>T): T ={
    val start = System.currentTimeMillis()
    val result = f
    val end = System.currentTimeMillis()
    val difference = end - start
    append(name, difference)
    result
  }

  def measureTime[T](name:String, count:Long,  f: =>T): T ={
    val start = System.currentTimeMillis()
    val result = f
    val end = System.currentTimeMillis()
    val difference = end - start
    append(name, difference, count)
    result
  }

  def append(name:String, intime:Long):this.type ={
    val count = countMap.getOrElse(name, 0L) + 1L
    val time = timeMap.getOrElse(name, 0L) + intime
    countMap = countMap.updated(name, count)
    timeMap = timeMap.updated(name, time)
    this
  }

  def append(name:String, intime:Long, incount:Long):this.type ={
    val count = countMap.getOrElse(name, 0L) + incount
    val time = timeMap.getOrElse(name, 0L) + intime
    countMap = countMap.updated(name, count)
    timeMap = timeMap.updated(name, time)
    this
  }

  def summary(): Unit ={
    timeMap.foreach{case(name, time)=>{
      println(s"Avg. time taken for name: ${name} is ${time.toDouble / (1000 * countMap(name)) } ms")
    }}
  }

  def xml():String={
    var result = "<EFFICIENCY>\n"

    timeMap.foreach{case(name, time)=>{
      val count = countMap(name)
      val insecs = time.toDouble / (count)
      result += "\n<NAME VALUE=\""+name+"\">"
         result += "\n<NSAMPLES VALUE=\""+count.toString+"\"/>"
         result += "\n<TOTAL MILISECONDS=\""+time+"\"/>"
         result += "\n<AVERAGE MILISECONDS=\""+insecs.toDouble+"\"/>"
      result += "\n</NAME>"
    }}

    result += "\n</EFFICIENCY>"
    result
  }
}

