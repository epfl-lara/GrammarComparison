package grammarcomp

package grammar.examples
import grammar._
import CFGrammar._
import GrammarReaders.GrammarImplicit

object Korenjak1966 extends QuizResult {
  import GrammarReaders._
  import QuizResult._
  
  implicit def MAX_GRADE = 0
  
  override def quizName = "Korenjack-Hopcroft 1966"    
    
  override def reference = bnfgrammar"""S1 -> a A C
  A -> b A B | a B
  B -> b
  C -> a""" 
  
  this add bnfgrammar"""S2 -> a D E 
  D -> b D F | a
  E -> b G
  F -> b
  G -> a""" not correct comment "G2 of Page 42"
}