package grammar

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec
import CFGrammar._
import org.scalatest.OptionValues._
import parsing._
import grammar.examples.Olshansky1977

class CYKParserTest extends FlatSpec with ShouldMatchers {
  import GrammarReaders._

  implicit val gctx = new GlobalContext()
  implicit val opctx = new ParseContext()
  
  import scala.language.implicitConversions
  implicit def strToSym(str: String): scala.Symbol = scala.Symbol(str)

  "The CYKParsers" should " tell if a sentence is recognized by a grammar" in {
    val grammar =
      grammar"""S -> NP VP
VP -> VP PP
VP -> V NP
VP -> eats
PP -> P NP
NP -> Det N
NP -> she
V -> eats
P -> with
N -> fish
N -> fork
Det -> a"""
    val parser = new CYKParser(grammar)
    val s1 = ("she eats a fish with a fork".split(" ") map Terminal.apply _).toList
    val s2 = ("she eats with fish with a fork".split(" ") map Terminal.apply _).toList
    val res1 = parser.parse(s1)
    val res2 = parser.parse(s2)    
    res1 should be(true)
    res2 should be(false)
  }

  "The CYKParsers" should "parse with the tree" in {
    val grammar =
      grammar"""S -> AS B | a
AS -> A S
A -> a
B -> b"""
    val s1 = ("a a a b b".split(" ") map Terminal.apply _).toList
    val s2 = ("a a b b b".split(" ") map Terminal.apply _).toList

    val S = Nonterminal("S")
    val AS = Nonterminal("AS")
    val A = Nonterminal("A")
    val B = Nonterminal("B")
    val a = Terminal("a")
    val b = Terminal("b")

    val parser = new CYKParser(grammar)
    parser.parse(s1) should be(true)
    parser.parse(s2) should be(false)
  }
  
  "The CYKParsers" should " tell if a sentence is recognized by a sentential form of Olshansky1977" in {
    val g = grammar"""S49 -> '(' D1 | '(' X2 | '(' S2c102
B1 -> '(' Bc481 | ')'
D1 -> '(' Dc471 | ')' | ')' S11
Bc481 -> '(' Bc481 B1 | ')' B1
Dc471 -> '(' Bc481 D1 | ')' D1
S2c102 -> '(' X2 X2 | '(' S2c102 X2
S11 -> '(' D1
S22 -> '(' X2 | '(' S2c102
X2 -> ')' | ')' S22""" 
    val parser = new CYKParser(g.cnfGrammar)    
    val word = "( ) ) )".split(" ") map Terminal.apply
    val sf = List(Nonterminal("D1"), Nonterminal("X2"))
    val res1 = parser.parseWithSententialForm(sf, word.toList)       
    res1 should be(true)    
  }
  
}