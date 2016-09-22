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
  def grammar = Grammar('E, List[Rules[String]](       
    'E ::= 'E ~ "+" ~ 'E | 'E ~ "*" ~ 'E |  "IDENTIFIER" | "(" ~ 'E ~ ")"  
  ))
}
