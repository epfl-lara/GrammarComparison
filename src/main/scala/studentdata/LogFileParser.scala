package studentdata

import grammar.utils._
import grammar._
import java.io._
import java.lang.management._
import scala.collection.JavaConversions._
import grammar.EBNFGrammar._

object LogFileParser {
  import StudentData._

  def main(args: Array[String]) = {
    val dirpath = args(0)
    val logfiles = readStudentData(dirpath)
    printStats(logfiles, println _)
  }

  def readStudentData(dirpath: String) = {
    val dir = new File(dirpath)
    if (!dir.isDirectory()) {
      throw new IllegalStateException("Not a directory: " + dirpath)
    } else {
      println("Reading logs from dir: " + dirpath)

      val filenames = dir.list()
      val logfiles = filenames.toList.map { fn =>
        var eventStrings = readEventStrings(dirpath + "/" + fn)
        val events = eventStrings.foldLeft(List[Event]()) { (acc, estr) =>
          parseEvent(estr.trim(), acc) match {
            case Some(event) =>
              //println("Event: "+event)
              acc :+ event
            case _ =>
              acc
          }
        }
        new LogFile(fn, events)
      }
      logfiles
    }
  }

  def readEventStrings(logfilename: String) = {
    val file = new File(logfilename)
    val br = new BufferedReader(new InputStreamReader(new FileInputStream(file)))
    var eventStrings = List[String]()
    var currEventString = ""
    var nextline = br.readLine()
    while (nextline != null) {
      if (nextline.startsWith("[<]") || nextline.startsWith("[>]")) {
        if (!currEventString.isEmpty())
          eventStrings :+= currEventString
        currEventString = nextline
      } else {
        currEventString += nextline
      }
      nextline = br.readLine()
    }
    eventStrings
  }

  def printStats(logfiles: List[LogFile], prln : String => Unit) {
    //compute total number do check events
    val (eqEvents, withGrammars) = logfiles.foldLeft((0, 0)) { (acc, lf) =>
      val eqEvents = lf.events.collect { case e: doCheck if e.exid == 1 => e }
      val withGrammars = eqEvents.count(_.code.isDefined)
      (acc._1 + eqEvents.size, acc._2 + withGrammars)
    }
    prln("# of equivalence queries: " + eqEvents)
    prln("# of equivalence queries with Grammars: " + withGrammars)
    val cexEvents = logfiles.map(_.events.count(_.isInstanceOf[CEx])).sum
    val provedEvents = logfiles.map(_.events.count(_.isInstanceOf[EquivProved])).sum
    prln("# of equivalence queries disproved: " + cexEvents)
    prln("# of equivalence queries proved: " + provedEvents)
    prln("# of equivalence queries that passes testcases: " + (eqEvents - (cexEvents + provedEvents)))

    //compute total number ambiguity events
    val (ambEvents, ambWithGrammars) = logfiles.foldLeft((0, 0)) { (acc, lf) =>
      val ambEvents = lf.events.collect { case e: checkAmbiguity => e }
      val withGrammars = ambEvents.count(_.code.isDefined)
      (acc._1 + ambEvents.size, acc._2 + withGrammars)
    }
    prln("# of ambiguity queries: " + ambEvents)
    prln("# of ambiguity queries with Grammars: " + ambWithGrammars)
    val ambExs = logfiles.map(_.events.count(_.isInstanceOf[AmbiguityEx])).sum
    val unambEvents = logfiles.map(_.events.count(_.isInstanceOf[UnambiguityProved])).sum
    prln("# of ambiguity queries disproved: " + ambExs)
    prln("# of ambiguity queries proved: " + unambEvents)
    prln("# of ambiguity queries that passes testcases: " + (ambEvents - (ambExs + unambEvents)))
  }

  /**
   * Removes quotes from the string
   */

  sealed abstract class Direction
  object ClientSend extends Direction
  object ServerSend extends Direction

  def toDirection(str: String) = {
    if (str == "<")
      Some(ClientSend)
    else if (str == ">")
      Some(ServerSend)
    else None
  }

  def trimQuotes(str: String): String = {
    val tstr = str.trim()
    val pstr = if (tstr.charAt(0) == '\"') {
      tstr.substring(1)
    } else tstr
    val fstr = if (pstr.last == '\"') {
      pstr.substring(0, pstr.size - 1)
    } else pstr
    fstr.trim()
  }

  def collectKeyValuePairs(data: String) = {
    var pairs = List[(String, String)]()
    var tokenStart = 0
    var i = 0
    while (i < data.length) {
      //go until a comma    
      while (i < data.length && data.charAt(i) != ',') {
        i += 1
      }
      val entry = data.substring(tokenStart, i)
      val parts = entry.split(":")
      val key = trimQuotes(parts(0))
      val value =
        //special handling for 'code' key. 
        if (key == "code") {
          i = data.length
          trimQuotes(data.substring(tokenStart + parts(0).size + 1, i))
        } else
          trimQuotes(parts(1))
      pairs :+= (key, value)
      i += 1
      tokenStart = i //start of the next token
    }
    pairs
  }

  def parseEvent(line: String, events: List[Event]): Option[Event] = {
    //println("Line : " + line)
    var splits = line.split("[\\[\\]{}]").filterNot(_.trim().isEmpty())
    val direction = toDirection(splits(0)).get
    val eventid = splits(1).toInt
    val time = splits(2)
    val data = splits(3)

    //println(s"""Eventid: $eventid Time: $time""")
    if (direction == ClientSend) {
      //data is a set of key:value pairs separated by ,
      val kvpairs = collectKeyValuePairs(data).toMap

      //for debugging
      //      kvpairs.foreach {
      //        case (k, v) =>
      //          println(s"""key: $k value: $v""")
      //      }
      if (kvpairs("action") == "doCheck") {
        //read rest of the fields here
        val exid = kvpairs("exerciseId").toInt
        val pid = kvpairs("problemId").toInt
        val code = kvpairs("code")

        val grammarStr = code.replace("\\n", "\n").replace("\\r", "\n").replace("\\\"", "\"").trim()
        val (bnfG, errstr) = (new GrammarParser()).parseGrammar(grammarStr.split("\n").toList)
        if (bnfG.isDefined)
          Some(doCheck(eventid, time, exid, pid, bnfG))
        else {
          //println("Cannot parse grammar: "+grammarStr)
          //throw new IllegalArgumentException(errstr)
          Some(doCheck(eventid, time, exid, pid, None))
        }
      } else if (kvpairs("action") == "checkAmbiguity") {
        val code = kvpairs("code")
        val grammarStr = code.replace("\\n", "\n").replace("\\r", "\n").replace("\\\"", "\"").trim()
        val (bnfG, errstr) = (new GrammarParser()).parseGrammar(grammarStr.split("\n").toList)
        if (bnfG.isDefined)
          Some(checkAmbiguity(eventid, time, bnfG))
        else {
          //println("Cannot parse grammar: "+grammarStr)
          //throw new IllegalArgumentException(errstr)
          Some(checkAmbiguity(eventid, time, None))
        }
      } else
        None
    } else {
      val doCheckIds = events.collect { case e: doCheck if e.exid == 1 => e.eventid }.toSet
      //here the event is a message sent from the server
      //if check if there is a doCheck event from the client with exercise id '1' which means "grammar" exercises
      if (doCheckIds.contains(eventid)) {
        if (data.contains("Correct."))
          Some(EquivProved(eventid, time))
        else if (data.contains("The grammar does not accept") ||
          data.contains("The grammar accepts"))
          Some(CEx(eventid, time))
        else None
      } else {
        val ambCheckIds = events.collect { case e: checkAmbiguity => e.eventid }.toSet
        if (ambCheckIds.contains(eventid)) {
          if (data.contains("The grammar is unambiguous"))
            Some(UnambiguityProved(eventid, time))
          else if (data.contains("at least two parse trees"))
            Some(AmbiguityEx(eventid, time))
          else
            None
        } else
          None
      }
    }
  }
}