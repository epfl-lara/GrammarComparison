package grammarcomp

package benchmarks
import grammar._
import grammar.CFGrammar._
import grammar.GrammarReaders.GrammarImplicit
import grammar.examples.QuizResult

object ToolGrammar extends Benchmark {
  import GrammarReaders._
  
  def benchmarkName = "ToolGrammar"
  def benchmarkSource = "lara-compliers-page"     
  def ebnfGrammar = bnfgrammar"""Goal	::=	MainObject ( ClassDeclaration )* 
MainObject	::=	object Identifier { def main '(' ')' : Unit = { ( Statement )* } }
ClassDeclaration	::=	class Identifier ( extends Identifier )? { ( VarDeclaration )* ( MethodDeclaration )* }
VarDeclaration	::=	var Identifier : Type ;
MethodDeclaration	::=	def Identifier '(' ( Identifier : Type ( , Identifier : Type )* )? ')' : Type = { ( VarDeclaration )* ( Statement )* return Expression ; }
Type	::=	Int [ ] | Bool | Int | String | Identifier
Statement	::=	{ ( Statement )* } 
  | if '(' Expression ')' Statement ( else Statement )? 
  | while '(' Expression ')' Statement 
  | println '(' Expression ')' ; 
  | Identifier = Expression ; 
  | Identifier [ Expression ] = Expression ;
Expression	::=	Expression ( && | '||' | == | < | '+' | - | '*' | / ) Expression 
  | Expression [ Expression ] 
  | Expression . length 
  | Expression . Identifier '(' ( Expression ( , Expression )* )? ')' 
  | INTEGER_LITERAL | " STRING_LITERAL " | true | false 
  | Identifier | this | new Int [ Expression ] | new Identifier '(' ')' 
  | ! Expression | '(' Expression ')'
Identifier	::=	IDENTIFIER"""
}
