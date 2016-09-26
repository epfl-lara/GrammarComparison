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
  case class ID(name: String) extends Token with TerminalClass
  case class LBrace() extends Token
  case class RBrace() extends Token
  
  val IDSentinel = ID("Sent")
  
  def grammar = Grammar('E, List[Rules[Token]](       
    'E ::= 'E ~ PlusToken() ~ 'E | 'E ~ TimesToken() ~ 'E |  IDSentinel | LBrace() ~ 'E ~ RBrace()  
  ))
  
  def ll1grammar = Grammar('E, List[Rules[Token]](       
    'E ::= 'Atom ~ 'Suf, 
    'Suf ::= PlusToken() ~ 'F | TimesToken() ~ 'F | epsilon(),
    'F ::= 'Atom ~ 'Suf,
    //'Suf2 ::= TimesToken() ~ 'F | PlusToken() ~ 'F | epsilon(),
    'Atom ::= IDSentinel | LBrace() ~ 'E ~ RBrace()
  ))
}
