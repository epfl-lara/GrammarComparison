package grammar

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec
import CFGrammar._
import org.scalatest.OptionValues._
import EBNFGrammar._
import BNFConverter._
import BNFGrammar._
import utils._

class GrammarTest extends FlatSpec with ShouldMatchers {
  import GrammarReaders._
  import equivalence._
  import parsing._

  implicit val gctx = new GlobalContext()
  implicit val pctx = new ParseContext()
  implicit val ectx = new EnumerationContext()
  implicit val eqctx = new EquivalenceCheckingContext()

  def testEquivalence(g1: Grammar[String], g2: Grammar[String]) = {
    new StudentGrammarEquivalenceChecker(g1).isEquivalentTo(g2)
  }

  "LLEpsilonEliminator.eliminateEpsilons" should " should work correctly for balanced parentheses" in {
    val g = bnfgrammar"""S -> '(' S ')' S | "" """
    val epg = LLEpslionEliminator.eliminateEpsilons(g.cfGrammar)
    GNFUtilities.isGNFGrammarLL2(GNFConverter.toGNF(epg)) should be(true)
    testEquivalence(g.cfGrammar, epg).isEmpty should be(true)
  }

  "LLEpsilonEliminator.eliminateEpsilons" should " should work correctly for expressions" in {
    val g = bnfgrammar"""E -> V Suf
				V -> ID | '(' E ')'					 
				Suf -> '+' E | '*' E | """""
    val epg = LLEpslionEliminator.eliminateEpsilons(g.cfGrammar)
    //println(epg)
    GNFUtilities.isGNFGrammarLL2(GNFConverter.toGNF(epg)) should be(true)
    testEquivalence(g.cfGrammar, epg).isEmpty should be(true)
  }
  
  "LLEpsilonEliminator.eliminateEpsilons" should " should work correctly for balanced paratheses with multiple parentheses" in {
    val g = bnfgrammar"""S -> "" | '(' S ')' S | '[' S ']' S"""
    val epg = LLEpslionEliminator.eliminateEpsilons(g.cfGrammar)
    //println(epg)
    GNFUtilities.isGNFGrammarLL2(GNFConverter.toGNF(epg)) should be(true)
    testEquivalence(g.cfGrammar, epg).isEmpty should be(true)
  }
  
  "LLEpsilonEliminator.eliminateEpsilons" should " should work correctly for simple while language" in {
    val g = bnfgrammar"""stmt ::= println '(' stringConst ',' ident ')'
				stmt ::= ident = expr | if '(' expr ')' stmt else stmt
				stmt ::= while '(' expr ')' stmt | '{' stmt* '}'"""
    val epg = LLEpslionEliminator.eliminateEpsilons(g.cfGrammar)
    //println(epg)
    GNFUtilities.isGNFGrammarLL2(GNFConverter.toGNF(epg)) should be(true)
    testEquivalence(g.cfGrammar, epg).isEmpty should be(true)
  }
}