package benchmarks
import grammar._
import EBNFGrammar._
import grammar.CFGrammar._
import grammar.GrammarReaders.GrammarImplicit

object ToolGrammarDSL {
  import GrammarDSL._   
  import scala.language.postfixOps
  
  def benchmarkName = "ToolGrammar using DSL"
  def benchmarkSource = "lara-compliers-page"     
  def grammar = Grammar('Goal, List[Rules[String]](
    'Goal ::= 'MainObject ~ 'ClassDecls,
    'ClassDecls ::= 'ClassDeclaration ~ 'ClassDecls | epsilon(),
    //'MainObject ->  "object" ~ 'Identifier ~ "{" ~ "def" ~ "main" ~ "(" ~ ")" ~ ":" ~ "Unit" ~ "=" ~ "{" ~ ('Statement*) ~ "}" ~ "}",
    'MainObject ::=  "object" ~ 'Identifier ~ "{" ~ 'Stmts ~ "}",
    'Stmts ::= 'Statement ~ 'Stmts | epsilon(),
    'ClassDeclaration	::=	"class" ~ 'Identifier ~ 'ExtendsOpt ~ "{" ~ 'VarDecs ~ 'MethodDecs ~ "}",
    'ExtendsOpt ::= "extends" ~ 'Identifier | epsilon(),
    'VarDecs ::= 'VarDeclaration ~ 'VarDecs | epsilon(), 
    'MethodDecs ::= 'MethodDeclaration ~ 'MethodDecs | epsilon(),
    'VarDeclaration	::= "var" ~ 'Identifier ~ ":" ~ 'Type ~ ";",
    'MethodDeclaration ::= "def" ~ 'Identifier ~ "(" ~ 'ParamsOpt ~ ")" ~ ":" ~ 'Type ~ "=" ~ "{" ~ 'VarDecs ~ 'Stmts ~ "return" ~ 'Expression ~ ";" ~ "}",
    'ParamsOpt ::= 'Identifier ~ ":" ~ 'Type ~ 'Params | epsilon(),
    'Params ::= ("," ~ 'Identifier ~ ":" ~ 'Type) ~ 'Params | epsilon(), 
    'Type	::=	"Int" ~ "[" ~ "]" | "Bool" | "Int" | "String" | 'Identifier,
    'Statement ::=	"{" ~ 'Stmts ~ "}"  
        | "if" ~ "(" ~ 'Expression ~ ")" ~ 'Statement ~ 'ElseOpt
        | "while" ~ "(" ~ 'Expression ~ ")" ~ 'Statement
        | "println" ~ "(" ~ 'Expression ~ ")" ~ ";"
        | 'Identifier ~ "=" ~ 'Expression ~ ";"
        | 'Identifier ~ "[" ~ 'Expression ~ "]" ~ "=" ~ 'Expression ~ ";"
        | "do" ~ "(" ~ 'Expression ~ ")" ~ ";",    
    'ElseOpt ::= "else"  ~ 'Statement | epsilon(), 
    'Expression ::= 'Expression ~ 'Op ~ 'Expression
      | 'Expression  ~ "[" ~ 'Expression ~ "]"  
      | 'Expression ~ "." ~ "length" 
      | 'Expression ~ "." ~ 'Identifier ~ "(" ~ 'ArgsOpt ~ ")" 
      | "INTEGER_LITERAL" 
      | "STRING_LITERAL" 
      | "true" | "false" | 'Identifier | "this" | "new" ~ "Int"~"["~'Expression~"]" | "new" ~ 'Identifier ~ "(" ~ ")" 
      | "!" ~ 'Expression | "(" ~ 'Expression ~ ")",
    'ArgsOpt ::= 'Expression ~ 'Args | epsilon(),
    'Args ::=  "," ~ 'Expression  ~  'Args | epsilon(),    
    'Op ::= "&&" | "||" | "==" | "<" | "+" | "-" | "*" | "/",
    'Identifier ::= "IDENTIFIER"     
  ))
}
