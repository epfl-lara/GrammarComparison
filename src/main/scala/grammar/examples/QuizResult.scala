package grammar.examples
import grammar._
import scala.collection.mutable.ArrayBuffer
import CFGrammar._
import EBNFGrammar._

object QuizResult {
  val yes = true
  val no = false
  val correct = 0
}

case class AnnotatedGrammar(grammar: BNFGrammar, grade: Option[Float], comment: Option[String])(quizResult: QuizResult, index: Option[Int]) {
  import QuizResult._
  def correct(b: Boolean): AnnotatedGrammar = this grade (if(b) quizResult.MAX_GRADE else 0)
  def grade(i : Float): AnnotatedGrammar = { val res = this.copy(grade=Some(i))(quizResult, index); if(index.nonEmpty) quizResult.student_grammars(index.get) = res; res}
  def comment(c : String): AnnotatedGrammar = { val res = this.copy(comment=Some(c))(quizResult, index); if(index.nonEmpty) quizResult.student_grammars(index.get) = res; res }
  def full(b: Boolean): AnnotatedGrammar = this grade (if(b) quizResult.MAX_GRADE else 0)
  def not(b: Int): AnnotatedGrammar = if(b == QuizResult.correct) this correct no else this
  def is(b: Int): AnnotatedGrammar = if(b == QuizResult.correct) this correct yes else this
}

abstract class QuizResult { quizResult =>   
   
  val student_grammars = ArrayBuffer[AnnotatedGrammar]()
  implicit def MAX_GRADE: Int
 
  def quizName : String
  def reference : BNFGrammar
  
  protected def add(g: BNFGrammar): AnnotatedGrammar = {
    val index = student_grammars.length
    val result = AnnotatedGrammar(g, None, None)(this, Some(index))
    student_grammars += result
    result
  }
  implicit class RichGrammar(g: BNFGrammar) {
    def annotated = AnnotatedGrammar(g, None, None)(quizResult, None)
  }
}