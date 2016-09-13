package studentdata

import grammar.utils._
import grammar._
import java.io._
import java.lang.management._
import scala.collection.JavaConversions._
import grammar.EBNFGrammar._

object StudentData {

  sealed abstract class Event
  case class doCheck(val eventid: Int, val time: String, exid: Int, probId: Int, code: Option[BNFGrammar[String]]) extends Event {
    //    override def toString(){
    //      
    //    }
  }
  case class checkAmbiguity(val eventid: Int, val time: String, code: Option[BNFGrammar[String]]) extends Event
  case class CEx(val eventid: Int, val time: String) extends Event
  case class EquivProved(val eventid: Int, val time: String) extends Event
  case class AmbiguityEx(val eventid: Int, val time: String) extends Event
  case class UnambiguityProved(val eventid: Int, val time: String) extends Event

  //case class EquivNotProved(val eventid: Int, val time: String) extends Event

  class LogFile(val filename: String, val events : List[Event]) {   
    lazy val equivEvents = events.collect{ case e: doCheck => e}
    //exercise 1 is english to grammar events
    lazy val exercise1Events = equivEvents.filter(_.exid == 1) 
  }
  //val logfiles = List[LogFile]()
 
}