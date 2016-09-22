package grammarcomp

package grammar.examples
import grammar._
import CFGrammar._
import GrammarReaders.GrammarImplicit

object ASequenceOfA extends QuizResult {
  import GrammarReaders._
  import QuizResult._

  implicit def MAX_GRADE = 0

  override def quizName = "If then else"

  override def reference = bnfgrammar"""S -> a S | a"""				

  this add bnfgrammar"""S -> a 
  | a a 
  | a a a 
  | a a a a 
  | a a a a a 
  | a a a a a a 
  | a a a a a a a 
  | a a a a a a a a 
  | a a a a a a a a a
  | a a a a a a a a a a
  | a a a a a a a a a a a
  | a a a a a a a a a a a a
  | a a a a a a a a a a a a a
  | a a a a a a a a a a a a a a
  | a a a a a a a a a a a a a a a
  | a a a a a a a a a a a a a a a a
  | a a a a a a a a a a a a a a a a a
  | a a a a a a a a a a a a a a a a a a
  | a a a a a a a a a a a a a a a a a a a 
  | a a a a a a a a a a a a a a a a a a a a
  | a a a a a a a a a a a a a a a a a a a a a
  | a a a a a a a a a a a a a a a a a a a a a a 
  | a a a a a a a a a a a a a a a a a a a a a a a"""   
}