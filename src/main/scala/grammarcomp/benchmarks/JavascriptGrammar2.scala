package grammarcomp

package benchmarks

import grammar.EBNFGrammar.BNFGrammar
import grammar.GrammarReaders


object JavascriptGrammar2 extends Benchmark {  
  import GrammarReaders._
  
  def benchmarkName = "JSGrammar-Simple"
    def benchmarkSource = "http://hepunx.rl.ac.uk/~adye/jsspec11/llr.htm"  
  
  def ebnfGrammar = bnfgrammar"""Program -> "" | Element Program
Element -> function Identifier '(' ParameterListOpt ')' CompoundStatement | Statement
ParameterListOpt -> "" | ParameterList
ParameterList -> Identifier | Identifier ',' ParameterList
CompoundStatement -> '{' Statements '}'
Statements -> "" | Statement Statements
Statement -> ';' | if Condition Statement | if Condition Statement else Statement | while Condition Statement | ForParen ';' ExpressionOpt ';' ExpressionOpt ')' Statement | ForBegin ';' ExpressionOpt ';' ExpressionOpt ')' Statement | ForBegin in Expression ')' Statement | break ';' | continue ';' | with '(' Expression ')' Statement | return ExpressionOpt ';' | CompoundStatement | VariablesOrExpression ;
Condition -> '(' Expression ')'
ForParen -> for '('
ForBegin -> ForParen VariablesOrExpression
VariablesOrExpression -> var Variables | Expression
Variables -> Variable | Variable ',' Variables
Variable -> Identifier | Identifier '=' AssignmentExpression
ExpressionOpt -> "" | Expression
Expression -> AssignmentExpression | AssignmentExpression ',' Expression
AssignmentExpression -> ConditionalExpression | ConditionalExpression AssignmentOperator AssignmentExpression
ConditionalExpression -> OrExpression | OrExpression '?' AssignmentExpression ':' AssignmentExpression
OrExpression -> AndExpression | AndExpression '||' OrExpression
AndExpression -> BitwiseOrExpression | BitwiseOrExpression '&&' AndExpression
BitwiseOrExpression -> BitwiseXorExpression | BitwiseXorExpression '|' BitwiseOrExpression
BitwiseXorExpression -> BitwiseAndExpression | BitwiseAndExpression ^ BitwiseXorExpression
BitwiseAndExpression -> EqualityExpression | EqualityExpression '&' BitwiseAndExpression
EqualityExpression -> RelationalExpression | RelationalExpression EqualityualityOperator EqualityExpression
RelationalExpression -> ShiftExpression | RelationalExpression RelationalationalOperator ShiftExpression
ShiftExpression -> AdditiveExpression | AdditiveExpression ShiftOperator ShiftExpression
AdditiveExpression -> MultiplicativeExpression | MultiplicativeExpression '+' AdditiveExpression | MultiplicativeExpression '-' AdditiveExpression
MultiplicativeExpression -> UnaryExpression | UnaryExpression MultiplicativeOperator MultiplicativeExpression
UnaryExpression -> MemberExpression | UnaryOperator UnaryExpression | '-' UnaryExpression | IncrementOperator MemberExpression | MemberExpression IncrementOperator | new Constructor | delete MemberExpression
Constructor -> this '.' ConstructorCall | ConstructorCall
ConstructorCall -> Identifier | Identifier '(' ArgumentListOpt ')' | Identifier . ConstructorCall
MemberExpression -> PrimaryExpression | PrimaryExpression '.' MemberExpression | PrimaryExpression '[' Expression ']' | PrimaryExpression '(' ArgumentListOpt ')'
ArgumentListOpt -> "" | ArgumentList
ArgumentList -> AssignmentExpression | AssignmentExpression ',' ArgumentList
PrimaryExpression -> '(' Expression ')' | Identifier | IntegerLiteral | FloatingPointLiteral | StringLiteral | false | true | null | this"""
}