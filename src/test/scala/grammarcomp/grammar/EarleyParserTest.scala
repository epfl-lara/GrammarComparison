package grammarcomp
package grammar

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import CFGrammar._
import grammarcomp.parsing._
import GrammarDSL._

class EarleyParserTest extends FlatSpec with Matchers {
  import GrammarReaders._

  implicit val gctx = new GlobalContext()
  implicit val opctx = new ParseContext()

  import scala.language.implicitConversions
  implicit def strToSym(str: String): scala.Symbol = scala.Symbol(str)
   
  "The EarleyParsers" should "tell if a sentence is recognized by a grammar" in {
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
  
  "The EarleyParsers" should "parse a simple grammar" in {
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

    val parser = new EarleyParser(grammar)
    parser.parse(s1) should be(true)
    parser.parse(s2) should be(false)
  }
  
  "The EarleyParsers" should "construct tree of a simple grammar" in {
    val grammar = Grammar('S, List[Rules[String]](
        'S ::= 'S ~ "f" ~ 'A
             | "a",
        'A ::= 'S ~ 'L,
        'L ::= 'S ~ 'L | epsilon()
      ))
    val s1 = ("a f a".split(" ") map Terminal.apply _).toList
    val s2 = ("a".split(" ") map Terminal.apply _).toList

    val parser = new EarleyParser(grammar)
    println(ParseTreeDSL.mapTree(parser.parseWithTree(s1).get))
    println(parser.getParseGraph)
    parser.parse(s1) should be(true)
  }
  
  "The EarleyParsers" should "parse the example grammar" in {
    val grammar =
      grammar"""S -> E
                E -> E p E
                E -> E m E
                E -> a"""
    val s1 = ("a p a p a".split(" ") map Terminal.apply _).toList

    val S = Nonterminal("S")
    val E = Nonterminal("E")
    val a = Terminal("a")
    val p = Terminal("p")
    val m = Terminal("m")

    val parser = new EarleyParser(grammar)
    val res1 = parser.parse(s1)
    println(parser.toString())
    println(parser.getParseGraph)
    res1 should be(true)
  }
}