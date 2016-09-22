package grammarcomp

package studentdata

import grammar.utils._
import grammar._
import java.io._
import java.lang.management._
import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer
import grammar.EBNFGrammar._
import grammar.exercises._
import equivalence.EquivalenceVerifier
import CFGrammar._

object StudentDataAnalyzer {
  import StudentData._

  def analyze(dir: String) = {
    implicit val gctx = new GlobalContext(logFilename = "student-data-analysis.log", 
        statsFilename = "student-data-analysis.stats")
    implicit val ambctx = new AmbiguityContext(consecWordsForAmbiguityCheck = 300)
    implicit val equivctx = new EquivalenceCheckingContext(nOfTestcases = 100, maxSize = 11,  timeOut = 900 * 1000)
    implicit val enumctx = new EnumerationContext()
    implicit val parsectx = new ParseContext()
    implicit val verictx = new EquivalenceVerificationContext(verificationTimeout = 10, //in seconds
      useTestcasesInVerification = true, testsForVerification = 100, maxSizeForVerification = 11,
      debugEquivVerifier = 0)      
    //for debugging
    Util.registerShutdownHook { gctx.stats.dumpStatsToFile }

    //select every grammar submitted for the exercise 1 and test them for equivalence
    val logfiles = LogFileParser.readStudentData(dir)
    val grammarDb = GrammarDatabase.readGrammarDatabase(
        new java.io.File("exercises-data/GrammarDatabase.xml"), "exercises-data/")
    
    var seenQueries = Set[(Grammar[String],Int)]() //a map of student grammar and problem id  

    //val trickyEvents = List(6) //3401,3139,7733)
    logfiles.foreach { lg =>
      lg.exercise1Events.foreach {
        case ev @ _ if ev.code.isDefined =>
          val msg = "Analyzing event " + ev.eventid
          println(msg)
          gctx.logMessage(msg)
          
          gctx.stats.updateCumStats(1, "QueriesWithGram")
          val studg = ev.code.get.cfGrammar          
          if (studg.cnfGrammar.rules.isEmpty) {
            //empty grammar cases
            gctx.logMessage("Grammar recognizes the empty language")
          } else {
            gctx.stats.updateCumStats(1, "QueriesWithParseGram")
            //check if the grammar and problem pair are seen before, if yes skip this
            if (seenQueries.contains(studg, ev.probId)) {
              gctx.stats.updateCounter(1, "DuplicateQueries")
            } else {
              //for stats
            	gctx.stats.updateCounter(1, "UniqueQueries")
            	gctx.stats.updateCounterStats(studg.nonTerminals.size, "Nonterms", "UniqueQueries")
            	gctx.stats.updateCounterStats(studg.rules.size, "Rules", "UniqueQueries")
          
              seenQueries += ((studg, ev.probId)) 
              //for stats
              val amb = !(new clients.AmbiguityChecker(studg)).checkAmbiguityInStudentGrammar().isEmpty
              if (amb)
                gctx.stats.updateCumStats(1, "AmbiguousSolution")

              val refg = grammarDb.grammarEntries.find(_.id == ev.probId).get.refGrammar
                            
//                println("Problem: " + ev.probId)
//                println("Reference Grammar: " + refg)
//                println("Analyzing Grammar: " + studg)

              val tm = new Stats.Timer()
              
              val equivChecker = new equivalence.StudentGrammarEquivalenceChecker(refg)
              val res = equivChecker.isEquivalentTo(studg)
              if (res.isEmpty) {
                gctx.stats.updateCumStats(1, "QueriesPassingTestcases")
                val verifier = new EquivalenceVerifier(refg, studg)
                verifier.proveEquivalence() match {
                  case Some(true) =>
                    gctx.stats.updateCumStats(1, "QueriesProved")
                    gctx.logMessage("The grammars are proved to be equivalent!")
                    //for stats
                    if (amb)
                      gctx.stats.updateCumStats(1, "AmbiguousGrammarVerified")
                  case _ =>                                       
                    gctx.stats.updateCounter(1, "QueriesUnproved")
                    gctx.stats.updateCounterStats(studg.nonTerminals.size, "UnprovedNonterms", "QueriesUnproved")
                    gctx.stats.updateCounterStats(studg.rules.size, "UnprovedRules", "QueriesUnproved")
                    gctx.logMessage("Cannot prove equivalence!")
                }
              } else
                gctx.stats.updateCumStats(1, "QueriesDisproved")
                
              gctx.stats.updateCounterTime(tm.timeWithoutGC, "TimePerQuery", "UniqueQueries")
            }
          }
        case _ =>
          ;
      }
    }
    //log the initial statistics for future reference
    gctx.logMessage("Initial Stats: ")
    var str = new String()
    LogFileParser.printStats(logfiles, str += _ + "\n")
    gctx.logMessage(str)
    gctx.stats.dumpStatsToFile()
    
    //print the smallest grammars
//    var msg = "Smallest grammar size: "+equivalence.EquivalenceVerfier.smallestSize +"\n"
//    msg += equivalence.EquivalenceVerfier.smallestGrammars.map{
//      case (g1,g2) => "Grammars: "+g1.toString + "\n"+g2.toString
//    }.mkString("\n")
//    gctx.logMessage(msg)
//    println(msg)
  }
}
