package grammarcomp
package grammar

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import CFGrammar._
import parsing._

class EarleyParserTest extends FlatSpec with Matchers {
  import GrammarReaders._

  implicit val gctx = new GlobalContext()
  implicit val opctx = new ParseContext()

  import scala.language.implicitConversions
  implicit def strToSym(str: String): scala.Symbol = scala.Symbol(str)
  
  "Equal EarleyItems" should "be equal" in {
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
    val parser = new EarleyParser(grammar)
    val res1 = parser.testEquals()
    res1 should be(true)
  }
  
  "The EarleyParsers" should " tell if a sentence is recognized by a grammar" in {
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
    val parser = new EarleyParser(grammar)
    val s1 = ("she eats a fish with a fork".split(" ") map Terminal.apply _).toList
    val s2 = ("she eats with fish with a fork".split(" ") map Terminal.apply _).toList
    val res1 = parser.parse(s1)
    val res2 = parser.parse(s2)
    res1 should be(true)
    res2 should be(false)
  }
}