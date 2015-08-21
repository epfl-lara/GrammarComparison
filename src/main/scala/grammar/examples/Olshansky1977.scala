package grammar.examples
import grammar._
import CFGrammar._
import GrammarReaders.GrammarImplicit

object Olshansky1977 extends QuizResult {
  import GrammarReaders._
  import QuizResult._
  
  implicit def MAX_GRADE = 0
  
  override def quizName = "Olshansky1977"    
    
  override def reference = bnfgrammar"""S1 -> '(' D
  D -> '(' B D | ')' | ')' S1
  B -> '(' B B | ')' """ 
  
  this add bnfgrammar"""S2 -> '(' X | '(' S2 X 
  X -> ')' | ')' S2""" comment "Example B in Page 14"
}