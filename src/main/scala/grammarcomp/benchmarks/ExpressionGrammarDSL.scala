package grammarcomp

package benchmarks
import grammar._
import EBNFGrammar._
import grammar.CFGrammar._
import grammar.GrammarReaders.GrammarImplicit

object ExprGrammarDSL {
  import GrammarDSL._   
  import scala.language.postfixOps
  
  def benchmarkName = "ToolGrammar using DSL"
  def benchmarkSource = "lara-compliers-page"
  
  sealed abstract class Token
  case class PlusToken() extends Token
  case class TimesToken() extends Token
  case class ID(name: String) extends Token
  case class LBrace() extends Token
  case class RBrace() extends Token
  
  object IDSentinel extends ID("") with TerminalClass {
    def contains(obj: Any) = obj.isInstanceOf[ID]
  }
  
  def grammar = Grammar('E, List[Rules[Token]](       
    'E ::= 'E ~ PlusToken() ~ 'E | 'E ~ TimesToken() ~ 'E |  IDSentinel | LBrace() ~ 'E ~ RBrace()  
  ))
}
