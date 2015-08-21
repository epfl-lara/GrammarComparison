package grammar
package utils

import scala.collection.mutable.{ Map => MutableMap }
import java.io._

object Stats {
  import java.lang.management._
  import scala.collection.JavaConversions._

  def getGarbageCollectionTime(): Long = {
    return ManagementFactory.getGarbageCollectorMXBeans().map(_.getCollectionTime()).sum
  }

  def resetPeakMemory() = {
    ManagementFactory.getMemoryPoolMXBeans().foreach(_.resetPeakUsage())
  }

  def peakMemory(): Long = {
    (ManagementFactory.getMemoryPoolMXBeans().map(_.getPeakUsage().getUsed()).sum) / 1000
  }

  /**
   * A timer that does not count garbage collection time
   */
  class Timer() {
    var initTime: Long = 0
    var initGCTime: Long = 0
    reset()

    def reset() {
      initTime = System.currentTimeMillis()
      initGCTime = getGarbageCollectionTime()
    }

    def elapsedTime(): Long = {
      (System.currentTimeMillis() - initTime)
    }

    def timeWithoutGC(): Long = {
      val timeWithGC = (System.currentTimeMillis() - initTime)
      timeWithGC - gcTime()
    }

    def gcTime(): Long = {
      (getGarbageCollectionTime() - initGCTime)
    }
  }
}

/**
 * A generic statistics class that provides:
 * (a) Temporal variables that change over time. We track the total sum and max of the values a variable takes over time
 * (b) Counters that are incremented over time. Variables can be associated with counters.
 *     We track the average value of a variable over time w.r.t to the counters with which it is associated.
 * (c) Variables that can be set to any value.
 */
class Stats(statsFilename: String) {
  import Stats._

  val varstats = MutableMap[String, Any]()
  val keystats = MutableMap[String, (Long, Long)]()
  val counterMap = MutableMap[String, Seq[String]]()
  var cumKeys = Seq[String]()
  var timekeys = Set[String]() //this may be inner, outer or cumkey

  private def updateStats(incr: Long, key: String, cname: Option[String]) = {
    val (cum, max) = keystats.getOrElse(key, {
      val init = (0: Long, 0: Long)
      keystats += (key -> (0, 0))

      if (cname.isDefined) {
        val presentKeys = counterMap(cname.get)
        counterMap.update(cname.get, presentKeys :+ key)
      } else {
        cumKeys :+= key
      }
      init
    })
    val newcum = cum + incr
    val newmax = if (max < incr) incr else max
    keystats.update(key, (newcum, newmax))
  }

  //a special method for adding times
  private def updateTimeStats(incr: Long, key: String, cname: Option[String]) = {
    if (!timekeys.contains(key))
      timekeys += key
    updateStats(incr, key, cname)
  }

  def updateCumStats(incr: Long, key: String) = updateStats(incr, key, None)
  def updateCumTime(incr: Long, key: String) = updateTimeStats(incr, key, None)
  def updateCounter(incr: Long, key: String) = {
    if (!counterMap.contains(key)) {
      counterMap.update(key, Seq())
    }
    //counters are considered as cumulative stats
    updateStats(incr, key, None)
  }
  def updateCounterStats(incr: Long, key: String, cname: String) = updateStats(incr, key, Some(cname))
  def updateCounterTime(incr: Long, key: String, cname: String) = updateTimeStats(incr, key, Some(cname))
  def updateVariable(varname: String, value: Any) {
    varstats.update(varname, value)
  }

  def getCum(key: String): Long = keystats(key)._1
  def getMax(key: String): Long = keystats(key)._2   
  
  def dumpStatsToFile() {
    val pw = new PrintWriter(new FileOutputStream(new File(statsFilename)))
    dumpStats(pw.println _)
    pw.close()
  }

  def dumpStats(prln: (String => Unit)) = {
    //Print values of variable
    varstats.foreach{
      case (k,v) => 
        prln(s"$k: $v")        
    }
    
    //Print cumulative stats
    cumKeys.foreach(key => {
      if (timekeys.contains(key)) {
        prln(key + ": " + (getCum(key).toDouble / 1000.0) + "s")
      } else
        prln(key + ": " + getCum(key))
    })

    //dump the averages and maximum of all stats associated with counters
    counterMap.keys.foreach((ckey) => {
      prln("### Statistics for counter: " + ckey + " ####")
      val counterval = getCum(ckey)
      val assocKeys = counterMap(ckey)
      assocKeys.foreach((key) => {
        if (timekeys.contains(key)) {
          prln("Avg." + key + ": " + (getCum(key).toDouble / (counterval * 1000.0)) + "s")
          prln("Max." + key + ": " + (getMax(key).toDouble / 1000.0) + "s")
        } else {
          prln("Avg." + key + ": " + (getCum(key).toDouble / counterval))
          prln("Max." + key + ": " + getMax(key))
        }
      })
    })   
  }
}