package benchmarks
import grammar._
import grammar.CFGrammar._
import grammar.EBNFGrammar._
import java.io._
import grammar.GrammarReaders.GrammarImplicit
import generators.GrammarBoundingHelper
import grammar.examples.QuizResult
import grammar.CNFConverter._

abstract class Benchmark { 
  def benchmarkName : String
  def benchmarkSource  : String
  def ebnfGrammar : BNFGrammar[String]
}

object C11Grammar1 extends Benchmark {
  def benchmarkName = "C11Grammar1"
  def benchmarkSource = "https://github.com/antlr/grammars-v4/blob/master/c/C.g4"    

  def ebnfGrammar = bnfgrammar"""translationUnit -> externalDeclaration | translationUnit externalDeclaration
primaryExpression -> Identifier | Constant | StringLiteral+ | '(' expression ')' | genericSelection | '(' compoundStatement ')'
genericSelection -> '_Generic' '(' assignmentExpression ',' genericAssocList ')'
genericAssocList -> genericAssociation | genericAssocList ',' genericAssociation
genericAssociation -> typeName ':' assignmentExpression | 'default' ':' assignmentExpression
postfixExpression -> primaryExpression | postfixExpression '[' expression ']' | postfixExpression '(' argumentExpressionList? ')' | postfixExpression '.' Identifier | postfixExpression '->' Identifier | postfixExpression '++' | postfixExpression '--' | '(' typeName ')' '{' initializerList '}' | '(' typeName ')' '{' initializerList ',' '}' | '(' typeName ')' '{' initializerList '}' | '(' typeName ')' '{' initializerList ',' '}'
argumentExpressionList -> assignmentExpression | argumentExpressionList ',' assignmentExpression
unaryExpression -> postfixExpression | '++' unaryExpression | '--' unaryExpression | unaryOperator castExpression | 'sizeof' unaryExpression | 'sizeof' '(' typeName ')' | '_Alignof' '(' typeName ')' | '&&' Identifier
unaryOperator -> '&' | '*' | '+' | '-' | '~' | '!'
castExpression -> unaryExpression | '(' typeName ')' castExpression | '(' typeName ')' castExpression
multiplicativeExpression -> castExpression | multiplicativeExpression '*' castExpression | multiplicativeExpression '/' castExpression | multiplicativeExpression '%' castExpression
additiveExpression -> multiplicativeExpression | additiveExpression '+' multiplicativeExpression | additiveExpression '-' multiplicativeExpression
shiftExpression -> additiveExpression | shiftExpression '<<' additiveExpression | shiftExpression '>>' additiveExpression
relationalExpression -> shiftExpression | relationalExpression '<' shiftExpression | relationalExpression '>' shiftExpression | relationalExpression '<=' shiftExpression | relationalExpression '>=' shiftExpression
equalityExpression -> relationalExpression | equalityExpression '==' relationalExpression | equalityExpression '!=' relationalExpression
andExpression -> equalityExpression | andExpression '&' equalityExpression
exclusiveOrExpression -> andExpression | exclusiveOrExpression '^' andExpression
inclusiveOrExpression -> exclusiveOrExpression | inclusiveOrExpression '|' exclusiveOrExpression
logicalAndExpression -> inclusiveOrExpression | logicalAndExpression '&&' inclusiveOrExpression
logicalOrExpression -> logicalAndExpression | logicalOrExpression '||' logicalAndExpression
conditionalExpression -> logicalOrExpression ('?' expression ':' conditionalExpression)?
assignmentExpression -> conditionalExpression | unaryExpression assignmentOperator assignmentExpression
assignmentOperator -> '=' | '*=' | '/=' | '%=' | '+=' | '-=' | '<<=' | '>>=' | '&=' | '^=' | '|='
expression -> assignmentExpression | expression ',' assignmentExpression
constantExpression -> conditionalExpression
declaration -> declarationSpecifiers initDeclaratorList? ';' | staticAssertDeclaration
declarationSpecifiers -> declarationSpecifier+
declarationSpecifiers2 -> declarationSpecifier+
declarationSpecifier -> storageClassSpecifier | typeSpecifier | typeQualifier | functionSpecifier | alignmentSpecifier
initDeclaratorList -> initDeclarator | initDeclaratorList ',' initDeclarator
initDeclarator -> declarator | declarator '=' initializer
storageClassSpecifier -> 'typedef' | 'extern' | 'static' | '_Thread_local' | 'auto' | 'register'
typeSpecifier -> ('void' | 'char' | 'short' | 'int' | 'long' | 'float' | 'double' | 'signed' | 'unsigned' | '_Bool' | '_Complex') | atomicTypeSpecifier | structOrUnionSpecifier | enumSpecifier | typedefName
structOrUnionSpecifier -> structOrUnion Identifier? '{' structDeclarationList '}' | structOrUnion Identifier
structOrUnion -> 'struct' | 'union'
structDeclarationList -> structDeclaration | structDeclarationList structDeclaration
structDeclaration -> specifierQualifierList structDeclaratorList? ';' | staticAssertDeclaration
specifierQualifierList -> typeSpecifier specifierQualifierList? | typeQualifier specifierQualifierList?
structDeclaratorList -> structDeclarator | structDeclaratorList ',' structDeclarator
structDeclarator -> declarator | declarator? ':' constantExpression
enumSpecifier -> 'enum' Identifier? '{' enumeratorList '}' | 'enum' Identifier? '{' enumeratorList ',' '}' | 'enum' Identifier
enumeratorList -> enumerator | enumeratorList ',' enumerator
enumerator -> enumerationConstant | enumerationConstant '=' constantExpression
enumerationConstant -> Identifier
atomicTypeSpecifier -> '_Atomic' '(' typeName ')'
typeQualifier -> 'const' | 'restrict' | 'volatile' | '_Atomic'
functionSpecifier -> ('inline' | '_Noreturn')
alignmentSpecifier -> '_Alignas' '(' typeName ')' | '_Alignas' '(' constantExpression ')'
declarator -> pointer? directDeclarator
directDeclarator -> Identifier | '(' declarator ')' | directDeclarator '[' typeQualifierList? assignmentExpression? ']' | directDeclarator '[' 'static' typeQualifierList? assignmentExpression ']' | directDeclarator '[' typeQualifierList 'static' assignmentExpression ']' | directDeclarator '[' typeQualifierList? '*' ']' | directDeclarator '(' parameterTypeList ')' | directDeclarator '(' identifierList? ')'
gccAttributeList -> gccAttribute (',' gccAttribute)* | ""
gccAttribute -> Identifier ('(' argumentExpressionList? ')')? | ""
pointer -> '*' typeQualifierList? | '*' typeQualifierList? pointer | '^' typeQualifierList? | '^' typeQualifierList? pointer
typeQualifierList -> typeQualifier | typeQualifierList typeQualifier
parameterTypeList -> parameterList | parameterList ',' '...'
parameterList -> parameterDeclaration | parameterList ',' parameterDeclaration
parameterDeclaration -> declarationSpecifiers declarator | declarationSpecifiers2 abstractDeclarator?
identifierList -> Identifier | identifierList ',' Identifier
typeName -> specifierQualifierList abstractDeclarator?
abstractDeclarator -> pointer | pointer? directAbstractDeclarator
directAbstractDeclarator -> '(' abstractDeclarator ')' | '[' typeQualifierList? assignmentExpression? ']' | '[' 'static' typeQualifierList? assignmentExpression ']' | '[' typeQualifierList 'static' assignmentExpression ']' | '[' '*' ']' | '(' parameterTypeList? ')' | directAbstractDeclarator '[' typeQualifierList? assignmentExpression? ']' | directAbstractDeclarator '[' 'static' typeQualifierList? assignmentExpression ']' | directAbstractDeclarator '[' typeQualifierList 'static' assignmentExpression ']' | directAbstractDeclarator '[' '*' ']' | directAbstractDeclarator '(' parameterTypeList? ')'
typedefName -> Identifier
initializer -> assignmentExpression | '{' initializerList '}' | '{' initializerList ',' '}'
initializerList -> designation? initializer | initializerList ',' designation? initializer
designation -> designatorList '='
designatorList -> designator | designatorList designator
designator -> '[' constantExpression ']' | '.' Identifier
staticAssertDeclaration -> '_Static_assert' '(' constantExpression ',' StringLiteral+ ')' ';'
statement -> labeledStatement | compoundStatement | expressionStatement | selectionStatement | iterationStatement | jumpStatement | 'volatile' '(' (logicalOrExpression (',' logicalOrExpression)*)? (':' (logicalOrExpression (',' logicalOrExpression)*)?)* ')' ';'
labeledStatement -> Identifier ':' statement | 'case' constantExpression ':' statement | 'default' ':' statement
compoundStatement -> '{' blockItemList? '}'
blockItemList -> blockItem | blockItemList blockItem
blockItem -> declaration | statement
expressionStatement -> expression? ';'
selectionStatement -> 'if' '(' expression ')' statement ('else' statement)? | 'switch' '(' expression ')' statement
iterationStatement -> 'while' '(' expression ')' statement | 'do' statement 'while' '(' expression ')' ';' | 'for' '(' expression? ';' expression? ';' expression? ')' statement | 'for' '(' declaration expression? ';' expression? ')' statement
jumpStatement -> 'goto' Identifier ';' | 'continue' ';' | 'break' ';' | 'return' expression? ';' | 'goto' unaryExpression ';'
compilationUnit -> translationUnit?
externalDeclaration -> functionDefinition | declaration | ';'
functionDefinition -> declarationSpecifiers? declarator declarationList? compoundStatement
declarationList -> declaration | declarationList declaration
Auto -> 'auto'
Break -> 'break'
Case -> 'case'
Char -> 'char'
Const -> 'const'
Continue -> 'continue'
Default -> 'default'
Do -> 'do'
Double -> 'double'
Else -> 'else'
Enum -> 'enum'
Extern -> 'extern'
Float -> 'float'
For -> 'for'
Goto -> 'goto'
If -> 'if'
Inline -> 'inline'
Int -> 'int'
Long -> 'long'
Register -> 'register'
Restrict -> 'restrict'
Return -> 'return'
Short -> 'short'
Signed -> 'signed'
Sizeof -> 'sizeof'
Static -> 'static'
Struct -> 'struct'
Switch -> 'switch'
Typedef -> 'typedef'
Union -> 'union'
Unsigned -> 'unsigned'
Void -> 'void'
Volatile -> 'volatile'
While -> 'while'
Alignas -> '_Alignas'
Alignof -> '_Alignof'
Atomic -> '_Atomic'
Bool -> '_Bool'
Complex -> '_Complex'
Generic -> '_Generic'
Imaginary -> '_Imaginary'
Noreturn -> '_Noreturn'
StaticAssert -> '_Static_assert'
ThreadLocal -> '_Thread_local'
LeftParen -> '('
RightParen -> ')'
LeftBracket -> '['
RightBracket -> ']'
LeftBrace -> '{'
RightBrace -> '}'
Less -> '<'
LessEqual -> '<='
Greater -> '>'
GreaterEqual -> '>='
LeftShift -> '<<'
RightShift -> '>>'
Plus -> '+'
PlusPlus -> '++'
Minus -> '-'
MinusMinus -> '--'
Star -> '*'
Div -> '/'
Mod -> '%'
And -> '&'
Or -> '|'
AndAnd -> '&&'
OrOr -> '||'
Caret -> '^'
Not -> '!'
Tilde -> '~'
Question -> '?'
Colon -> ':'
Semi -> ';'
Comma -> ','
Assign -> '='
StarAssign -> '*='
DivAssign -> '/='
ModAssign -> '%='
PlusAssign -> '+='
MinusAssign -> '-='
LeftShiftAssign -> '<<='
RightShiftAssign -> '>>='
AndAssign -> '&='
XorAssign -> '^='
OrAssign -> '|='
Equal -> '=='
NotEqual -> '!='
Arrow -> '->'
Dot -> '.'
Ellipsis -> '...'"""

}