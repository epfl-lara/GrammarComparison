package grammar.examples
import grammar._
import CFGrammar._
import GrammarReaders.GrammarImplicit

object Harrison1978First extends QuizResult {
  import GrammarReaders._
  import QuizResult._
  
  implicit def MAX_GRADE = 0
  
  override def quizName = "Harrison-Havel 1978"    
    
  override def reference = bnfgrammar"""S1 -> a A B
  A -> a A B | b
  B -> b""" 
  
  this add bnfgrammar"""S2 -> a C 
  C -> a C D | b D
  D -> b""" not correct comment "Page 177"
}