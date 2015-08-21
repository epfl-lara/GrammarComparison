package grammar.examples
import grammar._
import CFGrammar._
import GrammarReaders.GrammarImplicit

object IndirectRecursions extends QuizResult {
  import GrammarReaders._
  import QuizResult._

  implicit def MAX_GRADE = 0

  override def quizName = "Indirect Recursions"

    //the following grammar accepts a (b b)*  
  override def reference = bnfgrammar"""S -> A B | a
											A -> S B | b
        									B -> b"""

  this add bnfgrammar"""S -> A a | a
    					A -> B b | b
    					B -> S c | c B | c"""
}