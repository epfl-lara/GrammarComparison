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
import scala.collection.mutable.MultiMap
import scala.collection.mutable.HashMap

object RepairResult {
  sealed trait CNFFeedback[T]
  sealed trait GrammarFeedback[T]
  sealed trait BNFFeedback

  case class NoRepair[T](reason: String) extends GrammarFeedback[T] with CNFFeedback[T] with BNFFeedback {
    override def toString = reason
  }
  
  //CNF repair feedback    
  case class CNFRemove[T](r: Rule[T], repairRule : Option[Rule[T]] = None) extends CNFFeedback[T] {
    override def toString = " Remove rule " + r
  }
  case class CNFAdd[T](l: List[Rule[T]]) extends CNFFeedback[T] {
    override def toString = " Add rules " + l.mkString("\n")
  }
  case class CNFSplit[T](old: Rule[T], news: List[Rule[T]], repRule: Rule[T]) extends CNFFeedback[T] {
    override def toString = " Replace \n" + old + "\n by \n" + news.mkString("\n")
  }
  case class CNFExpand[T](old: Rule[T], news: List[Rule[T]]) extends CNFFeedback[T] {
    override def toString = " Expand \n" + old + "\n as \n" + news.mkString("\n")
  }

  //Grammar feedback 
  case class AddAllRules[T](l: List[Rule[T]]) extends GrammarFeedback[T] {
    override def toString = " Add rules " + rulesToStr(l)
  }
  case class RemoveRules[T](l: List[Rule[T]], repRules : Option[List[Rule[T]]] = None) extends GrammarFeedback[T] {
    override def toString = " Remove rules " + rulesToStr(l)
  }
  case class RefineRules[T](olds: List[Rule[T]], news: List[Rule[T]], repRules: List[Rule[T]]) extends GrammarFeedback[T] {
    override def toString = " Refine \n" + rulesToStr(olds) + "\n by replacing it by \n" + rulesToStr(news)
  }
  case class ExpandRules[T](olds: List[Rule[T]], news: List[Rule[T]]) extends GrammarFeedback[T] {
    override def toString = " Expand \n" + rulesToStr(olds) + "\n by replacing it by \n" + rulesToStr(news)
  }

  def cnfToGrammarFeedbacks[T](oldg: Grammar[T], newg: Grammar[T], cnfFBs: List[CNFFeedback[T]]) = cnfFBs map {
    case CNFAdd(rules) =>      
      val newrules = CNFConverter.removeCNFNonterminals(newg, rules)
      AddAllRules(newrules)

    case CNFRemove(rule, None) =>
      val newrules = CNFConverter.removeCNFNonterminals(oldg, List(rule))
      RemoveRules(newrules)
      
    case CNFRemove(rule, Some(repRule)) =>
      val newrules = CNFConverter.removeCNFNonterminals(oldg, List(rule))
      val repRules = CNFConverter.removeCNFNonterminals(oldg, List(repRule))
      RemoveRules(newrules, Some(repRules))

    case CNFSplit(iold, inews, repRule) =>
      val nolds = CNFConverter.removeCNFNonterminals(oldg, List(iold))
      val nnews = CNFConverter.removeCNFNonterminals(newg, inews)
      val nreps = CNFConverter.removeCNFNonterminals(newg, List(repRule))
      RefineRules(nolds, nnews, nreps)

    case CNFExpand(iold, inews) =>
      val nolds = CNFConverter.removeCNFNonterminals(oldg, List(iold))
      val nnews = CNFConverter.removeCNFNonterminals(newg, inews)
      ExpandRules(nolds, nnews)
      
    case n@NoRepair(_) => n
  }

  //BNF feedback 
  //TODO: do not know how to generate this
  /*case class AddAllBNFRules(l: List[BNFRule], s: String) extends BNFFeedback {
    override def toString = s + l.mkString("\n")
  }
  case class RemoveBNFRules(l: List[BNFRule], s: String) extends BNFFeedback {
    override def toString = s + l.mkString("\n")
  }
  case class ReplaceBNFRules(old: List[BNFRule], nu: List[BNFRule], s: String) extends BNFFeedback {
    override def toString = s + " replace \n" + old.mkString("\n") + "\n by \n" + nu.mkString("\n")
  }*/

  /*def toBNFFeedback(feedback: List[CNFFeedback], cnfG: Grammar) = {

    val genSymbols = CNFConverter.generatedSymbols
    val cnfContext = CNFConverter.enclosingContext
    val genRegexp = BNFConverter.generatedRegExp
    val bnfContext = BNFConverter.enclosingContext

    //println("Generated Regexp: "+genRegexp.mkString("\n"))
    //println("Generated Symbols: "+genSymbols.mkString("\n"))

    def cnfToGrammarRule(l: Rule[T]): Rule[T] = {
      val rightSide = l.rightSide.flatMap {
        case k: Nonterminal => genSymbols.getOrElse(k, List(k))
        case k => List(k)
      }
      val leftSide = l.leftSide
      cnfContext.getOrElse(leftSide, (l: List[Symbol]) => Rule[T](leftSide, rightSide))(rightSide)
    }
    def grammarToBNFrule(l: Rule[T]): BNFRule = {
      val rightSide = RegConcat(l.rightSide.map {
        case k: Symbol => {
          val re = genRegexp.getOrElse[RegExp](k, RegId(k.name): RegExp)
          //println("Regex generated: "+re+" contains: "+genRegexp.contains(k))
          re
        }
        case k => RegId(k.name)
      })
      //println("Generated right side for rule: "+ rightSide)
      val leftSide = l.leftSide
      bnfContext.getOrElse(leftSide, (k: RegExp) => BNFRule(RegId(leftSide.name), k))(rightSide)
    }

    val finalfeedback: List[BNFFeedback] = for (f <- feedback.toList) yield {
      f match {
        case c @ Comment(s) => c: BNFFeedback
        case AddAllRules(l, comment, d) =>              
          val bnfFixes = (l map cnfToGrammarRule map grammarToBNFrule).distinct          
          AddAllBNFRules(bnfFixes, comment, "")
          
        case RemoveRules(bad, comment, d) =>          
          val bnfFixes = (bad map cnfToGrammarRule map grammarToBNFrule).distinct                   
          RemoveBNFRules(bnfFixes, comment, "")
        
        case ReplaceRules(old, nu, comment) =>          
          val bnfOld = (old map cnfToGrammarRule map grammarToBNFrule).distinct
          val bnfNew = (nu map cnfToGrammarRule map grammarToBNFrule).distinct                   
          ReplaceBNFRules(bnfOld, bnfNew, comment)
      }
    }
    finalfeedback.toList
  }*/
}