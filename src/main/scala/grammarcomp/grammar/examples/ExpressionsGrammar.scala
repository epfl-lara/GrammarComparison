package grammarcomp

package grammar.examples
import grammar._
import CFGrammar._
import GrammarReaders.GrammarImplicit

object ExpressionsGrammar extends QuizResult {
  import GrammarReaders._
  import QuizResult._

  implicit def MAX_GRADE = 0

  override def quizName = "Expressions"

  override def reference = bnfgrammar"""E -> V Suf
				Suf -> '+' E | '*' E | ""
				V -> '(' E ')' | ID"""

//  this add bnfgrammar"""E -> V Suf
//				Suf -> '+' E | '*' E | ""
//				V -> '(' E ')' | ID"""
  
  this add bnfgrammar"""E -> ID Suf | '(' E ')' Suf
  			Suf -> '+' E | '*' E | "" """  
}