package grammarcomp

package grammar.examples
import grammar._
import CFGrammar._
import GrammarReaders.GrammarImplicit

object ThreeMod4 extends QuizResult {
  import GrammarReaders._
  import QuizResult._

  implicit def MAX_GRADE = 2

  override def quizName = "|W|_1 = 3 (mod 4)"

  override def reference = bnfgrammar"""S ->  0 S | 1 A 
				A ->  0 A | 1 B 
				B ->  0 B | 1 C 
    			C ->  0 C | 1 S | """""

  this add bnfgrammar"""S -> 0 S | 1 S | """""

}

object NotAllAlphabets extends QuizResult {
  import GrammarReaders._
  import QuizResult._

  implicit def MAX_GRADE = 2

  override def quizName = "words that do not contain all alphabets"

  override def reference = bnfgrammar"""S ->  A | B | C | ""
				A ->  a A | b A | ""
				B ->  b B | c B | ""
				C ->  c C | a C | "" """

  this add bnfgrammar"""S1 -> a S | a | b S4 | b | c S2 | c | ""
  		S2 -> a | b | c S | b S3 | a S | c
  		S3 -> b | c | b S | c S
  		S4 -> b S | b | c | a S | c S | a
  		S -> a S | b S | c S | a | b | c"""  

 //This required 200 testcases, 100 was not enough
//Correct DFA
/*S1 -> a S5 | a | b S4 | b | c S2 | c | ""
S5 -> b | a S5 | c | c S6 | b S7 | a
S6 -> a | c | c S6 | a S6
S3 -> b | c | b S3 | c S3
S7 -> a | b | a S7 | b S7
S2 -> a | b | c S2 | b S3 | a S6 | c
S4 -> b S4 | b | c | a S7 | c S3 | a*/
}