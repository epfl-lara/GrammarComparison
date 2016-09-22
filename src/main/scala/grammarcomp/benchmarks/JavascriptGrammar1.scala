package grammarcomp

package benchmarks

import grammar.EBNFGrammar.BNFGrammar
import grammar.GrammarReaders

object JavascriptGrammar1 extends Benchmark {
  /* From http://www-archive.mozilla.org/js/language/grammar14.html
   * This is an LR(1) grammar written by waldemar that describes the state of ECMAScript as of February 1999.
   * The grammar is complete except for semicolon insertion (the OptionalSemicolon grammar state can sometimes
   * reduce to «empty») and distinguishing RegularExpression from / and /=. Also, there is some controversy about
   * elision in array literals, so this feature has been omitted for now.
   * 
   * To compare with JavascriptGrammar3
   * */
  import GrammarReaders._
  
  def benchmarkName = "MozillaJSGrammar"
  def benchmarkSource = "http://www-archive.mozilla.org/js/language/grammar14.html"
  
  def ebnfGrammar = bnfgrammar"""Program -> TopStatements
  PrimaryExpressionnormal ->    SimpleExpression |  FunctionExpression |  ObjectLiteral
PrimaryExpressioninitial -> SimpleExpression
SimpleExpression ->    'this' |  'null' |  'true' |  'false' |  'Number' |  'String' |  'Identifier' |  'RegularExpression' |  ParenthesizedExpression |  ArrayLiteral
ParenthesizedExpression -> '(' ExpressionnormalallowIn  ')'
FunctionExpression ->    AnonymousFunction |  NamedFunction
ObjectLiteral ->    '{' '}' |  '{' FieldList '}'
FieldList ->    LiteralField |  FieldList ',' LiteralField
LiteralField -> 'Identifier' ':' AssignmentExpressionnormalallowIn 
ArrayLiteral ->    '[' ']' |  '[' ElementList ']'
ElementList ->    LiteralElement |  ElementList ',' LiteralElement
LiteralElement -> AssignmentExpressionnormalallowIn 
LeftSideExpressionnormal ->    CallExpressionnormal |  ShortNewExpression
LeftSideExpressioninitial ->    CallExpressioninitial |  ShortNewExpression
CallExpressionnormal ->    PrimaryExpressionnormal |  FullNewExpression |  CallExpressionnormal MemberOperator |  CallExpressionnormal Arguments
CallExpressioninitial ->    PrimaryExpressioninitial |  FullNewExpression |  CallExpressioninitial MemberOperator |  CallExpressioninitial Arguments
FullNewExpression -> 'new' FullNewSubexpression Arguments
ShortNewExpression -> 'new' ShortNewSubexpression
FullNewSubexpression ->    PrimaryExpressionnormal |  FullNewExpression |  FullNewSubexpression MemberOperator
ShortNewSubexpression ->    FullNewSubexpression |  ShortNewExpression
MemberOperator ->    '[' ExpressionnormalallowIn  ']' |  '.' 'Identifier'
Arguments ->    '(' ')' |  '(' ArgumentList ')'
ArgumentList ->    AssignmentExpressionnormalallowIn  |  ArgumentList ',' AssignmentExpressionnormalallowIn 
PostfixExpressionnormal ->    LeftSideExpressionnormal |  LeftSideExpressionnormal '++' |  LeftSideExpressionnormal '--'
PostfixExpressioninitial ->    LeftSideExpressioninitial |  LeftSideExpressioninitial '++' |  LeftSideExpressioninitial '--'
UnaryExpressionnormal ->    PostfixExpressionnormal |  'delete' LeftSideExpressionnormal |  'void' UnaryExpressionnormal |  'typeof' UnaryExpressionnormal |  '++' LeftSideExpressionnormal |  '--' LeftSideExpressionnormal |  '+' UnaryExpressionnormal |  '-' UnaryExpressionnormal |  '~' UnaryExpressionnormal |  '!' UnaryExpressionnormal
UnaryExpressioninitial ->    PostfixExpressioninitial |  'delete' LeftSideExpressionnormal |  'void' UnaryExpressionnormal |  'typeof' UnaryExpressionnormal |  '++' LeftSideExpressionnormal |  '--' LeftSideExpressionnormal |  '+' UnaryExpressionnormal |  '-' UnaryExpressionnormal |  '~' UnaryExpressionnormal |  '!' UnaryExpressionnormal
MultiplicativeExpressionnormal ->    UnaryExpressionnormal |  MultiplicativeExpressionnormal '*' UnaryExpressionnormal |  MultiplicativeExpressionnormal '/' UnaryExpressionnormal |  MultiplicativeExpressionnormal '%' UnaryExpressionnormal
MultiplicativeExpressioninitial ->    UnaryExpressioninitial |  MultiplicativeExpressioninitial '*' UnaryExpressionnormal |  MultiplicativeExpressioninitial '/' UnaryExpressionnormal |  MultiplicativeExpressioninitial '%' UnaryExpressionnormal
AdditiveExpressionnormal ->    MultiplicativeExpressionnormal |  AdditiveExpressionnormal '+' MultiplicativeExpressionnormal |  AdditiveExpressionnormal '-' MultiplicativeExpressionnormal
AdditiveExpressioninitial ->    MultiplicativeExpressioninitial |  AdditiveExpressioninitial '+' MultiplicativeExpressionnormal |  AdditiveExpressioninitial '-' MultiplicativeExpressionnormal
ShiftExpressionnormal ->    AdditiveExpressionnormal |  ShiftExpressionnormal '<<' AdditiveExpressionnormal |  ShiftExpressionnormal '>>' AdditiveExpressionnormal |  ShiftExpressionnormal '>>>' AdditiveExpressionnormal
ShiftExpressioninitial ->    AdditiveExpressioninitial |  ShiftExpressioninitial '<<' AdditiveExpressionnormal |  ShiftExpressioninitial '>>' AdditiveExpressionnormal |  ShiftExpressioninitial '>>>' AdditiveExpressionnormal
RelationalExpressionnormalallowIn ->    ShiftExpressionnormal |  RelationalExpressionnormalallowIn '<' ShiftExpressionnormal |  RelationalExpressionnormalallowIn '>' ShiftExpressionnormal |  RelationalExpressionnormalallowIn '<=' ShiftExpressionnormal |  RelationalExpressionnormalallowIn '>=' ShiftExpressionnormal |  RelationalExpressionnormalallowIn 'instanceof' ShiftExpressionnormal |  RelationalExpressionnormalallowIn 'in' ShiftExpressionnormal
RelationalExpressioninitialallowIn ->    ShiftExpressioninitial |  RelationalExpressioninitialallowIn '<' ShiftExpressionnormal |  RelationalExpressioninitialallowIn '>' ShiftExpressionnormal |  RelationalExpressioninitialallowIn '<=' ShiftExpressionnormal |  RelationalExpressioninitialallowIn '>=' ShiftExpressionnormal |  RelationalExpressioninitialallowIn 'instanceof' ShiftExpressionnormal |  RelationalExpressioninitialallowIn 'in' ShiftExpressionnormal
RelationalExpressionnormalnoIn ->    ShiftExpressionnormal |  RelationalExpressionnormalnoIn '<' ShiftExpressionnormal |  RelationalExpressionnormalnoIn '>' ShiftExpressionnormal |  RelationalExpressionnormalnoIn '<=' ShiftExpressionnormal |  RelationalExpressionnormalnoIn '>=' ShiftExpressionnormal |  RelationalExpressionnormalnoIn 'instanceof' ShiftExpressionnormal
RelationalExpressioninitialnoIn ->    ShiftExpressioninitial |  RelationalExpressioninitialnoIn '<' ShiftExpressionnormal |  RelationalExpressioninitialnoIn '>' ShiftExpressionnormal |  RelationalExpressioninitialnoIn '<=' ShiftExpressionnormal |  RelationalExpressioninitialnoIn '>=' ShiftExpressionnormal |  RelationalExpressioninitialnoIn 'instanceof' ShiftExpressionnormal
EqualityExpressionnormalallowIn ->    RelationalExpressionnormalallowIn |  EqualityExpressionnormalallowIn '==' RelationalExpressionnormalallowIn |  EqualityExpressionnormalallowIn '!=' RelationalExpressionnormalallowIn |  EqualityExpressionnormalallowIn '===' RelationalExpressionnormalallowIn |  EqualityExpressionnormalallowIn '!==' RelationalExpressionnormalallowIn
EqualityExpressionnormalnoIn ->    RelationalExpressionnormalnoIn |  EqualityExpressionnormalnoIn '==' RelationalExpressionnormalnoIn |  EqualityExpressionnormalnoIn '!=' RelationalExpressionnormalnoIn |  EqualityExpressionnormalnoIn '===' RelationalExpressionnormalnoIn |  EqualityExpressionnormalnoIn '!==' RelationalExpressionnormalnoIn
EqualityExpressioninitialallowIn ->    RelationalExpressioninitialallowIn |  EqualityExpressioninitialallowIn '==' RelationalExpressionnormalallowIn |  EqualityExpressioninitialallowIn '!=' RelationalExpressionnormalallowIn |  EqualityExpressioninitialallowIn '===' RelationalExpressionnormalallowIn |  EqualityExpressioninitialallowIn '!==' RelationalExpressionnormalallowIn
EqualityExpressioninitialnoIn ->    RelationalExpressioninitialnoIn |  EqualityExpressioninitialnoIn '==' RelationalExpressionnormalnoIn |  EqualityExpressioninitialnoIn '!=' RelationalExpressionnormalnoIn |  EqualityExpressioninitialnoIn '===' RelationalExpressionnormalnoIn |  EqualityExpressioninitialnoIn '!==' RelationalExpressionnormalnoIn
BitwiseAndExpressionnormalallowIn ->    EqualityExpressionnormalallowIn |  BitwiseAndExpressionnormalallowIn '&' EqualityExpressionnormalallowIn
BitwiseAndExpressionnormalnoIn ->    EqualityExpressionnormalnoIn |  BitwiseAndExpressionnormalnoIn '&' EqualityExpressionnormalnoIn
BitwiseAndExpressioninitialallowIn ->    EqualityExpressioninitialallowIn |  BitwiseAndExpressioninitialallowIn '&' EqualityExpressionnormalallowIn
BitwiseAndExpressioninitialnoIn ->    EqualityExpressioninitialnoIn |  BitwiseAndExpressioninitialnoIn '&' EqualityExpressionnormalnoIn
BitwiseXorExpressionnormalallowIn ->    BitwiseAndExpressionnormalallowIn |  BitwiseXorExpressionnormalallowIn '^' BitwiseAndExpressionnormalallowIn
BitwiseXorExpressionnormalnoIn ->    BitwiseAndExpressionnormalnoIn |  BitwiseXorExpressionnormalnoIn '^' BitwiseAndExpressionnormalnoIn
BitwiseXorExpressioninitialallowIn ->    BitwiseAndExpressioninitialallowIn |  BitwiseXorExpressioninitialallowIn '^' BitwiseAndExpressionnormalallowIn
BitwiseXorExpressioninitialnoIn ->    BitwiseAndExpressioninitialnoIn |  BitwiseXorExpressioninitialnoIn '^' BitwiseAndExpressionnormalnoIn
BitwiseOrExpressionnormalallowIn ->    BitwiseXorExpressionnormalallowIn |  BitwiseOrExpressionnormalallowIn '|' BitwiseXorExpressionnormalallowIn
BitwiseOrExpressionnormalnoIn ->    BitwiseXorExpressionnormalnoIn |  BitwiseOrExpressionnormalnoIn '|' BitwiseXorExpressionnormalnoIn
BitwiseOrExpressioninitialallowIn ->    BitwiseXorExpressioninitialallowIn |  BitwiseOrExpressioninitialallowIn '|' BitwiseXorExpressionnormalallowIn
BitwiseOrExpressioninitialnoIn ->    BitwiseXorExpressioninitialnoIn |  BitwiseOrExpressioninitialnoIn '|' BitwiseXorExpressionnormalnoIn
LogicalAndExpressionnormalallowIn ->    BitwiseOrExpressionnormalallowIn |  LogicalAndExpressionnormalallowIn '&&' BitwiseOrExpressionnormalallowIn
LogicalAndExpressionnormalnoIn ->    BitwiseOrExpressionnormalnoIn |  LogicalAndExpressionnormalnoIn '&&' BitwiseOrExpressionnormalnoIn
LogicalAndExpressioninitialallowIn ->    BitwiseOrExpressioninitialallowIn |  LogicalAndExpressioninitialallowIn '&&' BitwiseOrExpressionnormalallowIn
LogicalAndExpressioninitialnoIn ->    BitwiseOrExpressioninitialnoIn |  LogicalAndExpressioninitialnoIn '&&' BitwiseOrExpressionnormalnoIn
LogicalOrExpressionnormalallowIn ->    LogicalAndExpressionnormalallowIn |  LogicalOrExpressionnormalallowIn '||' LogicalAndExpressionnormalallowIn
LogicalOrExpressionnormalnoIn ->    LogicalAndExpressionnormalnoIn |  LogicalOrExpressionnormalnoIn '||' LogicalAndExpressionnormalnoIn
LogicalOrExpressioninitialallowIn ->    LogicalAndExpressioninitialallowIn |  LogicalOrExpressioninitialallowIn '||' LogicalAndExpressionnormalallowIn
LogicalOrExpressioninitialnoIn ->    LogicalAndExpressioninitialnoIn |  LogicalOrExpressioninitialnoIn '||' LogicalAndExpressionnormalnoIn
ConditionalExpressionnormalallowIn ->    LogicalOrExpressionnormalallowIn |  LogicalOrExpressionnormalallowIn '?' AssignmentExpressionnormalallowIn ':' AssignmentExpressionnormalallowIn
ConditionalExpressionnormalnoIn ->    LogicalOrExpressionnormalnoIn |  LogicalOrExpressionnormalnoIn '?' AssignmentExpressionnormalnoIn ':' AssignmentExpressionnormalnoIn
ConditionalExpressioninitialallowIn ->    LogicalOrExpressioninitialallowIn |  LogicalOrExpressioninitialallowIn '?' AssignmentExpressionnormalallowIn ':' AssignmentExpressionnormalallowIn
ConditionalExpressioninitialnoIn ->    LogicalOrExpressioninitialnoIn |  LogicalOrExpressioninitialnoIn '?' AssignmentExpressionnormalnoIn ':' AssignmentExpressionnormalnoIn
AssignmentExpressionnormalallowIn ->    ConditionalExpressionnormalallowIn |  LeftSideExpressionnormal '=' AssignmentExpressionnormalallowIn |  LeftSideExpressionnormal CompoundAssignment AssignmentExpressionnormalallowIn
AssignmentExpressionnormalnoIn ->    ConditionalExpressionnormalnoIn |  LeftSideExpressionnormal '=' AssignmentExpressionnormalnoIn |  LeftSideExpressionnormal CompoundAssignment AssignmentExpressionnormalnoIn
AssignmentExpressioninitialallowIn ->    ConditionalExpressioninitialallowIn |  LeftSideExpressioninitial '=' AssignmentExpressionnormalallowIn |  LeftSideExpressioninitial CompoundAssignment AssignmentExpressionnormalallowIn
AssignmentExpressioninitialnoIn ->    ConditionalExpressioninitialnoIn |  LeftSideExpressioninitial '=' AssignmentExpressionnormalnoIn |  LeftSideExpressioninitial CompoundAssignment AssignmentExpressionnormalnoIn
CompoundAssignment ->    '*=' |  '/=' |  '%=' |  '+=' |  '-=' |  '<<=' |  '>>=' |  '>>>=' |  '&=' |  '^=' |  '|='
ExpressionnormalallowIn ->    AssignmentExpressionnormalallowIn |  ExpressionnormalallowIn ',' AssignmentExpressionnormalallowIn
ExpressionnormalnoIn ->    AssignmentExpressionnormalnoIn |  ExpressionnormalnoIn ',' AssignmentExpressionnormalnoIn
ExpressioninitialallowIn ->    AssignmentExpressioninitialallowIn |  ExpressioninitialallowIn ',' AssignmentExpressionnormalallowIn
ExpressioninitialnoIn ->    AssignmentExpressioninitialnoIn |  ExpressioninitialnoIn ',' AssignmentExpressionnormalnoIn
OptionalExpression ->    ExpressionnormalallowIn  |  ""
StatementnoShortIf ->    EmptyStatement |  ExpressionStatement OptionalSemicolon |  VariableDefinition OptionalSemicolon |  Block |  LabeledStatementnoShortIf |  IfStatementnoShortIf |  SwitchStatement |  DoStatement OptionalSemicolon |  WhileStatementnoShortIf |  ForStatementnoShortIf |  WithStatementnoShortIf |  ContinueStatement OptionalSemicolon |  BreakStatement OptionalSemicolon |  ReturnStatement OptionalSemicolon |  ThrowStatement OptionalSemicolon |  TryStatement
Statementfull ->    EmptyStatement |  ExpressionStatement OptionalSemicolon |  VariableDefinition OptionalSemicolon |  Block |  LabeledStatementfull |  IfStatementfull |  SwitchStatement |  DoStatement OptionalSemicolon |  WhileStatementfull |  ForStatementfull |  WithStatementfull |  ContinueStatement OptionalSemicolon |  BreakStatement OptionalSemicolon |  ReturnStatement OptionalSemicolon |  ThrowStatement OptionalSemicolon |  TryStatement
OptionalSemicolon -> ';'
EmptyStatement -> ';'
ExpressionStatement -> ExpressioninitialallowIn
VariableDefinition -> 'var' VariableDeclarationListallowIn
VariableDeclarationListallowIn ->    VariableDeclarationallowIn |  VariableDeclarationListallowIn ',' VariableDeclarationallowIn
VariableDeclarationListnoIn ->    VariableDeclarationnoIn |  VariableDeclarationListnoIn ',' VariableDeclarationnoIn
VariableDeclarationallowIn -> 'Identifier' VariableInitializerallowIn
VariableDeclarationnoIn -> 'Identifier' VariableInitializernoIn
VariableInitializerallowIn ->    "" |  '=' AssignmentExpressionnormalallowIn
VariableInitializernoIn ->    "" |  '=' AssignmentExpressionnormalnoIn
Block -> '{' BlockStatements '}'
BlockStatements ->    "" |  BlockStatementsPrefix
BlockStatementsPrefix ->    Statementfull |  BlockStatementsPrefix Statementfull
LabeledStatementnoShortIf -> 'Identifier' ':' StatementnoShortIf
LabeledStatementfull -> 'Identifier' ':' Statementfull
IfStatementfull ->    'if' ParenthesizedExpression Statementfull |  'if' ParenthesizedExpression StatementnoShortIf 'else' Statementfull
IfStatementnoShortIf -> 'if' ParenthesizedExpression StatementnoShortIf 'else' StatementnoShortIf
SwitchStatement ->    'switch' ParenthesizedExpression '{' '}' |  'switch' ParenthesizedExpression '{' CaseGroups LastCaseGroup '}'
CaseGroups ->    "" |  CaseGroups CaseGroup
CaseGroup -> CaseGuards BlockStatementsPrefix
LastCaseGroup -> CaseGuards BlockStatements
CaseGuards ->    CaseGuard |  CaseGuards CaseGuard
CaseGuard ->    'case' ExpressionnormalallowIn  ':' |  'default' ':'
DoStatement -> 'do' Statementfull 'while' ParenthesizedExpression
WhileStatementnoShortIf -> 'while' ParenthesizedExpression StatementnoShortIf
WhileStatementfull -> 'while' ParenthesizedExpression Statementfull
ForStatementnoShortIf ->    'for' '(' ForInitializer ';' OptionalExpression ';' OptionalExpression ')' StatementnoShortIf |  'for' '(' ForInBinding 'in' ExpressionnormalallowIn  ')' StatementnoShortIf
ForStatementfull ->    'for' '(' ForInitializer ';' OptionalExpression ';' OptionalExpression ')' Statementfull |  'for' '(' ForInBinding 'in' ExpressionnormalallowIn  ')' Statementfull
ForInitializer ->    "" |  ExpressionnormalnoIn |  'var' VariableDeclarationListnoIn
ForInBinding ->    LeftSideExpressionnormal |  'var' VariableDeclarationnoIn
WithStatementnoShortIf -> 'with' ParenthesizedExpression StatementnoShortIf
WithStatementfull -> 'with' ParenthesizedExpression Statementfull
ContinueStatement -> 'continue' OptionalLabel
BreakStatement -> 'break' OptionalLabel
OptionalLabel ->    "" |  'Identifier'
ReturnStatement -> 'return' OptionalExpression
ThrowStatement -> 'throw' ExpressionnormalallowIn 
TryStatement ->    'try' Block CatchClauses |  'try' Block FinallyClause |  'try' Block CatchClauses FinallyClause
CatchClauses ->    CatchClause |  CatchClauses CatchClause
CatchClause -> 'catch' '(' 'Identifier' ')' Block
FinallyClause -> 'finally' Block
FunctionDefinition -> NamedFunction
AnonymousFunction -> 'function' FormalParametersAndBody
NamedFunction -> 'function' 'Identifier' FormalParametersAndBody
FormalParametersAndBody -> '(' FormalParameters ')' '{' TopStatements '}'
FormalParameters ->    "" |  FormalParametersPrefix
FormalParametersPrefix ->    FormalParameter |  FormalParametersPrefix ',' FormalParameter
FormalParameter -> 'Identifier'
TopStatements ->    "" |  TopStatementsPrefix
TopStatementsPrefix ->    TopStatement |  TopStatementsPrefix TopStatement
TopStatement ->    Statementfull |  FunctionDefinition"""
}