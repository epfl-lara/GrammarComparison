package engine

import grammar.examples._
import grammar.utils._
import grammar._
import repair._
import parsing._
import CFGrammar._
import EBNFGrammar._
import BNFConverter._
import equivalence._
import generators._
import grammar.exercises._
import EvaluateQuiz._
import clients._
import java.io._
import generators.RandomAccessGenerator.Element
import scala.collection.mutable.ListBuffer
import benchmarks._
import AmbiguityChecker._
import scala.concurrent.duration._
import scala.concurrent.util._
import java.util.concurrent._

class RandomErrorGeneration(g: Grammar, seed: Long) {
  val rand = new java.util.Random(seed)
  //also make sure we don't generate the same error again
  var errorData = Set(List[Int]()) //this is a number, pair or a triple 
  
  def getErrgen(level: Int) = if (level == 1)
    createLevel1Errors _
  else if (level == 2)
    createLevel2Errors _
  else if (level == 3)
    createLevel3Errors _
  else
    throw new IllegalStateException("Illegal level value: " + level)

  def createErrors(level: Int, minErrLength: Int, maxErrLength: Int) = {
    val errgen = getErrgen(level)
    var errg: Grammar = null
    var break = false
    var errLen = 0
    do {
      val (eg, eData) = errgen()
      if (!errorData.contains(eData) && !eg.cnfGrammar.rules.isEmpty) {
        val mismatchLen = RandomAccessGeneratorUtil.firstMismatchSize(g.cnfGrammar, eg.cnfGrammar, maxErrLength - 1)
        if (mismatchLen.isDefined && mismatchLen.get >= minErrLength) {
          errorData += eData
          errg = eg
          errLen = mismatchLen.get
          break = true
        }        
      }
    } while (!break)
    
    val ops = (CNFConverter.removeUnproductiveRules _
      andThen CNFConverter.removeUnreachableRules)
    (ops(errg), errLen)
  }

  def createLevel1Errors() = {
    //randomly choose a rule and remove it from the grammar
    val ruleNum = rand.nextInt(g.rules.size)
    val rule = g.rules(ruleNum)        
    (Grammar(g.start, g.rules.filterNot(_ == rule)), List(ruleNum))
  }

  lazy val rulesWithArity2 = g.rules.filter(_.rightSide.size > 2)
  def createLevel2Errors() = {
    //randomly choose a non-terminal of a right-side and remove it from the grammar    
    val ruleNum = rand.nextInt(rulesWithArity2.size)
    val rule = rulesWithArity2(ruleNum)
    val rhsIndex = rand.nextInt(rule.rightSide.size)
    val newrule = Rule(rule.leftSide, Util.removeAtIndex(rule.rightSide, rhsIndex))

    val errg = Grammar(g.start, g.rules.map {
      case `rule` => newrule
      case rl => rl
    })
    (errg, List(ruleNum, rhsIndex))
  }

  lazy val selfRecursive = g.rules.filter { case rl => rl.rightSide.contains(rl.leftSide) }
  /**
   * randomly choose a self-recursive rule, randomly pick a RHS symbol and duplicate the symbol
   * ignoring one rule
   */
  def createLevel3Errors() = {

    val ruleNum = rand.nextInt(selfRecursive.size)
    val rule @ Rule(left, right) = selfRecursive(ruleNum)
    val recNonterms = rule.rightSide.zipWithIndex.filter(_._1 == left)
    val recIndex = rand.nextInt(recNonterms.size)
    val rhsIndex = recNonterms(recIndex)._2

    //randomly pick a rule from the left nonterm and drop it
    val leftRules = g.nontermToRules(left)
    val leftRuleIndex = rand.nextInt(leftRules.size)
    val errRules = Util.removeAtIndex(leftRules, leftRuleIndex)

    //create a new-non-terminal with errrules, but, in which 'left' is replaced by 'newLeft'
    val newLeft = Nonterminal(Util.freshName(Some(left.name)))
    val newRules = CFGrammar.replace(errRules, Map(left -> newLeft)) //this is to avoid indirect left recursion,which is not supported by antlr

    val modrule = Rule(left, (right.take(rhsIndex) :+ newLeft) ++ right.drop(rhsIndex + 1))
    val errg = Grammar(g.start, g.rules.map {
      case `rule` => modrule
      case rl => rl
    } ++ newRules)
    (errg, List(ruleNum, rhsIndex, leftRuleIndex))
  }
}
  
  