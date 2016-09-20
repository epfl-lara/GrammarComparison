/**
 * File:    GrammarComparator.scala
 * Date:    20/5/2013
 * Author:  Mikaël Mayer
 * Purpose: Compares two grammars
 */
package repair
import grammar._
import CFGrammar._
import EBNFGrammar._
import generators._
import parsing._
import equivalence._
import utils._
import scala.collection.mutable.ListBuffer
import RepairResult._

/**
 * Takes a reference grammar in any form
 */
class Repairer[T](equivChecker: EquivalenceChecker[T])
	(implicit gctx: GlobalContext,
	    context: RepairContext) {
  import RepairResult._
  /**
   * Takes a grammar in CNF form.
   * Returns a list of feedbacks which could be used to make the grammar correct using native symbols.
   * @param cnfG The grammar in CNF format
   */
  def repair(cnfG: Grammar[T], result: EquivalenceResult[T]): Grammar[T] = {
    require(isInCNF(cnfG))

    //perform repair recursively until the grammar becomes equivalent    
    def recRepair(g: Grammar[T], result: EquivalenceResult[T]): Grammar[T] = {
      val newg = result match {
        case NotEquivalentNotAcceptedBySolution(unparsableWord) =>
          val newg = repairSubsetGrammar(g, unparsableWord)
          newg
        case NotEquivalentGeneratedBySolution(ungeneratableWord) =>          
          val newg = repairSupersetGrammar(g, ungeneratableWord)
          newg
        case _ =>
          g //nothing to repair otherwise
      }
      //convert to CNF form             
      val newresult = equivChecker.isEquivalentTo(newg)
      newresult match {
        case List() =>
          newg
        case head :: _ =>
          recRepair(newg, head)
      }
    }
    val simpg = cnfG.fromCNF 
    recRepair(simpg, result).cnfGrammar       
  }

  /**
   * Applies repair one step and constructs a feedback
   */
  def hint(cnfG: Grammar[T], result: EquivalenceResult[T]) = {
    require(isInCNF(cnfG))

    val simpg = cnfG.fromCNF 
    val feedback = result match {
      case NotEquivalentNotAcceptedBySolution(unparsableWord) =>
        repairSubsetStep(simpg, unparsableWord)

      case NotEquivalentGeneratedBySolution(ungeneratableWord) =>
        repairSupersetStep(simpg, ungeneratableWord)

      case _ =>
        //no repair is required or possible here.
        NoRepair[T]("Invalid Arguments.")
    }
    val newg = applyFixes(simpg, List(feedback))    
    (newg, List(feedback))
  }

  /* def genSupersetFeedback(oldg: Grammar, newg: Grammar, changeMap: Map[Rule, List[Rule]]) = {
    //compute removed and replaced rules and generate feedback
    val removedRules = changeMap.keys.filter(key => changeMap(key).isEmpty).toList
    val replacedRules = changeMap.collect {
      case (key, value) if !value.isEmpty => (key, value)
    }
    val removeFeedbacks = if (removedRules.isEmpty) List()
    						else generateRemoveFeedback(oldg, removedRules)
    val replaceFeedbacks = generateReplaceFeedback(oldg, newg, replacedRules)
    removeFeedbacks ++ replaceFeedbacks
  }*/

  def repairSupersetGrammar(initGrammar: Grammar[T], initWord: List[Terminal[T]]): Grammar[T] = {

    var iterations = 0
    var wordsRuledout = List[List[Terminal[T]]]()

    def rec(g: Grammar[T], ungenWord: List[Terminal[T]]): Grammar[T] = {

      //check if we need to abort

      if (context.debugRepair > 0)
        println("Blocking acceptance of string: " + wordToString(ungenWord))
      iterations += 1
      wordsRuledout :+= ungenWord

      val feedback = repairSupersetStep(g, ungenWord)
      val newg = applyFixes(g, List(feedback))

      if (context.debugSupersetIteration) {
        println("Grammar: " + g)
        println("Feedback: " + feedback)
        println("[Superset] New Grammar: " + newg)
        scala.io.StdIn.readLine()
      }

      equivChecker.isSubset(newg) match {
        case List() => 
          newg
        case head :: _ =>
          rec(newg, head)
      }      
    }

    if (context.debugSupersetIteration)
      println("[Superset] Initial Grammar: " + initGrammar)
    val finalg = rec(initGrammar, initWord)

    if (gctx.enableStats) {
      println("####statistics#####")
      println("Iterations: " + iterations)
      println("Words ruled out: " + wordsToString(wordsRuledout))
    }
    finalg
  }

  def repairSupersetStep(g: Grammar[T], ungenWord: List[Terminal[T]]): GrammarFeedback[T] = {
    if (ungenWord == List()) {
      //handle epsilon specially
      val epsilonRule = Rule(g.start, List[Symbol[T]]())
      RemoveRules(List(epsilonRule))
    } else {
      val feedback = (new ContextBasedSuperSetRepair(g, ungenWord, equivChecker)).eliminateAParseTree()
      feedback
    }
  }

  def applyFixes(ing: Grammar[T], fixes: List[GrammarFeedback[T]]): Grammar[T] = {
    val newg = fixes.foldLeft(ing)((g, fix) => fix match {
      case AddAllRules(rules) =>
        Grammar(g.start, g.rules ++ rules)
      case RemoveRules(rules, _) =>
        Grammar(g.start, g.rules.filterNot(rules.contains))
      case RefineRules(olds, news, _) =>
        Grammar(g.start, g.rules.filterNot(olds.contains) ++ news)
      case ExpandRules(olds, news) =>
        Grammar(g.start, g.rules.filterNot(olds.contains) ++ news)
      case NoRepair(_) =>
        //do nothing
        g
    })
    if (newg.rules.exists(_.rightSide.contains(newg.start))) {
      //here we need to renormalize the grammar
      newg.fromCNF 
    } else {
      newg
    }
  }

  val subsetRepair = new SubsetRepair(equivChecker)
  def repairSubsetStep(g: Grammar[T], unparsableWord: List[Terminal[T]]) = {
    if (unparsableWord == List()) {
      //handle epsilon specially
      val epsilonRule = Rule(g.start, List[Symbol[T]]())
      AddAllRules(List(epsilonRule))
    } else {
      val feedback = subsetRepair.makeParsable(g, unparsableWord)
      feedback
    }
  }

  def repairSubsetGrammar(initg: Grammar[T], initWord: List[Terminal[T]]): Grammar[T] = {

    def rec(g: Grammar[T], unparsableWord: List[Terminal[T]]): Grammar[T] = {

      if (context.debugRepair > 0)
        println("Enabling acceptance of string: " + wordToString(unparsableWord))

      val feedback = repairSubsetStep(g, unparsableWord)
      val newg = applyFixes(g, List(feedback))       

      if (context.debugSubsetIteration) {
        println("[Subset] New Grammar: " + Grammar(g.start, newg.rules))
      }      
      equivChecker.isSuperset(newg) match {
        case List() =>
          newg
        case head :: _ =>
          rec(newg, head)  
      }
    }
    if (context.debugSubsetIteration)
      println("[Subset] Initial Grammar: " + initg)
    rec(initg, initWord)
  }
}