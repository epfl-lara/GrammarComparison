package grammarcomp
package grammar

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import CFGrammar._
import parsing._

class GrammarUtilsTest extends FlatSpec with Matchers {
  import GrammarReaders._
  import GrammarUtils._

  implicit val opctx = new GlobalContext()
  
  import scala.language.implicitConversions
  implicit def strToSym(str: String): scala.Symbol = scala.Symbol(str)

  "GrammarUtils" should " tell why a grammar is not in LL(1) because of the first sets" in {
    val grammar =
      grammar"""S -> A S B
A -> a
B -> b
S -> a"""
    println(isLL1WithFeedback(grammar))    
    isLL1(grammar) should be(false)
  }

  "GrammarUtils" should " tell why a grammar is not in LL(1) because first and follow are not disjoint" in {
    val grammar =
      grammar"""S -> A S B | ""
A -> a
B -> a
S -> c"""
    println(isLL1WithFeedback(grammar))
    isLL1(grammar) should be(false)
  }

  "LL1Parser" should "Parse LL1 grammars" in {
    val grammar = grammar"""Sp -> Int V
V -> => Int V | , Int W | $$
W -> => Int V | , Int W"""

    GrammarUtils.isLL1(grammar) should be(true)

    val parsed = new LL1Parser(grammar).parseWithTree(("Int" :: "," :: "Int" :: "=>" :: "Int" :: "$" :: Nil).map(Terminal.apply))
    parsed should not be 'empty

    parsed.get should equal(
      Node(Rule(Nonterminal("Sp"), List(Terminal("Int"), Nonterminal("V"))),
        List(Leaf(Terminal("Int")),
          Node(Rule(Nonterminal("V"), List(Terminal(","), Terminal("Int"), Nonterminal("W"))),
            List(Leaf(Terminal(",")), Leaf(Terminal("Int")), Node(
              Rule(Nonterminal("W"), List(Terminal("=>"), Terminal("Int"), Nonterminal("V"))),
              List(Leaf(Terminal("=>")), Leaf(Terminal("Int")), Node(
                Rule(Nonterminal("V"), List(Terminal("$"))),
                List(Leaf(Terminal("$")))))))))))
  }
}  
