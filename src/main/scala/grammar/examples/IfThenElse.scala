package grammar.examples
import grammar._
import CFGrammar._
import GrammarReaders.GrammarImplicit

object IfThenElse extends QuizResult {
  import GrammarReaders._
  import QuizResult._

  implicit def MAX_GRADE = 0

  override def quizName = "If then else"

  override def reference = bnfgrammar"""S ::= pstmt | if '(' c ')' S Opt
    Opt -> "" | else S"""				
}