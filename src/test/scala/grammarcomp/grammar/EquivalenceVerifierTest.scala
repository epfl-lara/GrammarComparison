package grammarcomp
package grammar

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import CFGrammar._
import EBNFGrammar._
import equivalence._
import grammar.examples._
import equivalence.EquivalenceVerfier._

class EquivalenceVerifierTest extends FlatSpec with Matchers {
  import GrammarReaders._

  implicit val opctx = new EquivalenceVerificationContext(testsForVerification = 50,
    maxSizeForVerification = 11)
  implicit val gctx = new GlobalContext()
  implicit val ectx = new EnumerationContext()
  
  import scala.language.implicitConversions
  implicit def strToSym(str: String): scala.Symbol = scala.Symbol(str)

  "EquivalenceVerifier.maxMinWord" should " should be computed correctly for Olshansky1977" in {
    val verifier = new EquivalenceVerifier(Olshansky1977.reference.cfGrammar,
      Olshansky1977.student_grammars(0).grammar.cfGrammar)
    //println("GNF: "+verifier.genum)
    wordToString(verifier.maxMinWord) should equal("( ) )") //this is after conversion to GNF etc.       
  }

  "EquivalenceVerifier.getMinWord" should " should work correctly for Olshansky1977" in {
    val verifier = new EquivalenceVerifier(Olshansky1977.reference.cfGrammar,
      Olshansky1977.student_grammars(0).grammar.cfGrammar)    

    //first testcase
    wordToString(verifier.getMinWord(List(Nonterminal("D1")), Terminal("("))) should equal("( ) )")

    //second testcase
    val sf = List(Nonterminal("D1"), Nonterminal("S22"))
    wordToString(verifier.getMinWord(sf, Terminal("("))) should equal("( ) ) ( )")

    //third testcase
    val sf2 = List(Nonterminal("B1"), Nonterminal("D1"), Nonterminal("S11"))
    wordToString(verifier.getMinWord(sf2, Terminal("("))) should equal("( ) ) ) ( )")
  }

  "EquivalenceVerifier.genWords" should " should work correctly for Olshansky1977" in {
    val verifier = new EquivalenceVerifier(Olshansky1977.reference.cfGrammar,
      Olshansky1977.student_grammars(0).grammar.cfGrammar)
    val prefix = Terminal("(")
    val sf = List(Nonterminal("D1"), Nonterminal("S22"))
    val words = verifier.genWords(sf, 50, prefix)
    //println(wordsToString(words))
    (words.size == 50 && words.forall(_.size <= 11) && words.forall(_.head == prefix)) should be(true)

    //second testcase
    val sf2 = List(Nonterminal("D1"), PRNonterminal(Nonterminal("B1"), prefix))
    val words2 = verifier.genWords(sf2, 50, prefix)
    //println(wordsToString(words2))
    (words2.size < 50 && words2.forall(_.size <= 11) && words2.forall(_.head == prefix)) should be(true)

    //third testcase
    val sf3 = List(Nonterminal("D1"), PRNonterminal(Nonterminal("X2"), prefix))
    val words3 = verifier.genWords(sf3, 50, prefix)
    words3.isEmpty should be(true)
  }

  "EquivalenceVerifier.parseWithSententialForms" should " should work correctly for Olshansky1977" in {
    val verifier = new EquivalenceVerifier(Olshansky1977.reference.cfGrammar,
      Olshansky1977.student_grammars(0).grammar.cfGrammar)
    val prefix = Terminal("(")
    val sf = List(Nonterminal("D1"), Nonterminal("X2"))
    val words = verifier.genWords(sf, 50, prefix)
    val parseRes = verifier.parseWithSententialForms(List(sf), words)

    //every word should be parsable
    parseRes.get._2.isEmpty should be(true)

    //second testcase
    val sf2 = List(Nonterminal("D1"))
    val parseRes2 = verifier.parseWithSententialForms(List(sf2), words)
    parseRes2.get._2.size should equal(50)

    //third testcase
    val sf3 = List(Nonterminal("D1"), PRNonterminal(Nonterminal("X2"), prefix))
    val parseRes3 = verifier.parseWithSententialForms(List(sf3), words)
    parseRes3.get._2.size should equal(50)

    //fourth testcase
    val sf4 = List(Nonterminal("D1"), PRNonterminal(Nonterminal("X2"), Terminal(")")))
    val parseRes4 = verifier.parseWithSententialForms(List(sf4), words)
    parseRes4.get._2.isEmpty should be(true)

    //fifth testcase
    val sf5 = List(PRNonterminal(Nonterminal("D1"), prefix), PRNonterminal(Nonterminal("X2"), Terminal(")")))
    val parseRes5 = verifier.parseWithSententialForms(List(sf5), words)
    parseRes5.get._2.isEmpty should be(true)
  }

  "EquivalenceVerifier.proveEquivalence" should " should establish equivalence for Olshansky1977" in {
    val verifier = new EquivalenceVerifier(Olshansky1977.reference.cfGrammar,
      Olshansky1977.student_grammars(0).grammar.cfGrammar)
    verifier.proveEquivalence() should equal(Some(true))
  }

  "EquivalenceVerifier.proveEquivalence" should " should establish equivalence for Korenjak1966" in {
    val verifier = new EquivalenceVerifier(Korenjak1966.reference.cfGrammar,
      Korenjak1966.student_grammars(0).grammar.cfGrammar)
    verifier.proveEquivalence() should equal(Some(true))
  }

  "EquivalenceVerifier.proveEquivalence" should " should establish equivalence for Harrison1978" in {
    val verifier = new EquivalenceVerifier(Harrison1978First.reference.cfGrammar,
      Harrison1978First.student_grammars(0).grammar.cfGrammar)
    verifier.proveEquivalence() should equal(Some(true))
  }

  "EquivalenceVerifier.proveEquivalence" should " should produce correct output on grammars that are not equivalent" in {

    val g = grammar"""S1 -> '(' D
    					D -> '(' B D | ')' 
    					B -> '(' B B | ')' """

    (new EquivalenceVerifier(g, Olshansky1977.student_grammars(0).grammar.cfGrammar)).proveEquivalence() should equal(Some(false))

    val g2 = grammar"""S1 -> '(' S ')' | S S | """""
    (new EquivalenceVerifier(g, g2).proveEquivalence()) should equal(Some(false))

    val g3 = grammar"""S1 -> '(' S ')' | """""
    (new EquivalenceVerifier(g2, g3).proveEquivalence()) should equal(Some(false))
  }

  "EquivalenceVerifier.proveEquivalence" should " should establish equivalence of expression grammars" in {
    val g1 = bnfgrammar"""E -> V Suf
				V -> ID | '(' E ')'					 
				Suf -> '+' E | '*' E | """""
    val g2 = bnfgrammar"""E -> '(' E ')' Suf | ID Suf  					 
						  Suf -> '+' E | '*' E | """""
    val verifier = new EquivalenceVerifier(g1.cfGrammar, g2.cfGrammar)
    verifier.proveEquivalence() should equal(Some(true))
  }
  
  "EquivalenceVerifier.proveEquivalence" should " should establish equivalence of identical statement grammars" in {
    val g = bnfgrammar"""stmt ::= println '(' stringConst ',' ident ')'
				stmt ::= ident = expr | while '(' expr ')' stmt | '{' stmt* '}'"""    
    val verifier = new EquivalenceVerifier(g.cfGrammar, g.cfGrammar)
    verifier.proveEquivalence() should equal(Some(true))
  }
}