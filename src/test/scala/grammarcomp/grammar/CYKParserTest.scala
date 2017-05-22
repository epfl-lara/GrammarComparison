package grammarcomp
package grammar

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import CFGrammar._
import grammarcomp.parsing._

class CYKParserTest extends FlatSpec with Matchers {
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

  "The CYKParsers" should "should parse a simple grammar" in {
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

  // tests for parsing sentential forms  
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

  // tests for checking parsing of 2NF grammars 
  "The CYKParsers" should " should be able to create trees on a 2NF expression grammar" in {
    val g = grammar"""E -> V Suf
				Suf -> '+' E | '*' E | ""
				V -> '(' E ')' | ID"""
    //println("2NF grammar: "+g.twonfGrammar)
    val parser = new CYKParser(g.twonfGrammar)
    val word = "ID + ID * ID * ( ID )".split(" ").map(tok => Terminal(tok.trim())).toList
    //println("Parsing string: "+word.mkString(" "))
    val trees = parser.parseWithTrees(word)
    //println("Tree: "+ParseTreeUtils.parseTreetoString(trees(0)))
    val res1 = trees.isInstanceOf[Parsed[String]]
    res1 should be(true)
  }

  "The CYKParsers" should " should be able to create trees on a 2NF if-then-else grammar" in {
    val g = grammar"""S ::= pstmt | if '(' c ')' S Opt
    Opt -> "" | else S"""
    //println("2NF grammar: " + g.twonfGrammar)
    val parser = new CYKParser(g.twonfGrammar)
    val word = "if ( c ) if ( c ) pstmt else pstmt".split(" ").map(tok => Terminal(tok.trim())).toList
    //println("Parsing string: " + word.mkString(" "))
    val trees = parser.parseWithTrees(word)
    //println("Tree: " + ParseTreeUtils.parseTreetoString(trees(0)))
    val res1 = trees.isInstanceOf[Parsed[String]]
    res1 should be(true)
  }

  "The CYKParsers" should " should be able to handle unit productions" in {
    val g = grammar"""E ::= F
    F ::= ID"""
    val parser = new CYKParser(g.twonfGrammar)
    val word = List(Terminal("ID"))
    val fb = parser.parseWithTrees(word)
    //println("Tree: " + ParseTreeUtils.parseTreetoString(trees(0)))
    val res1 =
      fb match {
        case s: Parsed[String] =>
          s.parseTrees(0) match {
            case PNode(Rule(Nonterminal('E), _), _) => true
            case _                                  => false
          }
      }
    res1 should be(true)
  }

  "The CYKParsers" should " should be able to handle epsilon productions" in {
    val g = grammar"""E ::= G A
    G ::= A F
    F ::= ID
    A ::= """""
    val parser = new CYKParser(g.twonfGrammar)
    val word = List(Terminal("ID"))
    val fb = parser.parseWithTrees(word)
    //    /println("Tree: " + ParseTreeUtils.parseTreetoString(trees(0)))
    val res1 =
      fb match {
        case s: Parsed[String] =>
          s.parseTrees(0) match {
            case PNode(Rule(Nonterminal('E), _), _) => true
            case _                                  => false
          }
      }
    res1 should be(true)
  }

  // add tests for testing feedback
  "LL(1) parser" should " should provide correct feedback on LL(1) Expression Grammar" in {
    val g = grammar"""E -> V Suf
				Suf -> '+' E | '*' E | ""
				V -> '(' E ')' | ID"""
    val word = "ID + ID ID * ( ID )".split(" ").map(tok => tok.trim()).toList
    //println("Parsing string: "+word.mkString(" "))   
    val err = ParseTreeUtils.parseWithTrees(g, word)
    //println("Feedback: " + err)
    val res1 = err match {
      case err @ LL1Error(Nonterminal('Suf), Some(Terminal("ID"))) => true
      case _ => false
    }
    res1 should be(true)
  }

  "CYK parser" should " should provide correct feedback on Expression Grammar" in {
    val g = grammar"""E -> E '+' E | E '*' E | '(' E ')' | ID"""
    val word = "ID + ID ID * ( ID )".split(" ").map(tok => tok.trim()).toList
    println("Parsing string: "+word.mkString(" "))   
    val err = ParseTreeUtils.parseWithTrees(g, word)
    //println("Feedback: " + err)
    val res1 = err match {
      case err @ CYKError(fdb) => true
      case _                        => false
    }
    res1 should be(true)
  }
}