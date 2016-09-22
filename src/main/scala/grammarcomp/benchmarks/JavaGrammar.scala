package grammarcomp

package benchmarks
import grammar._
import grammar.CFGrammar._
import java.io._
import grammar.GrammarReaders.GrammarImplicit
import generators.GrammarBoundingHelper
import grammar.examples.QuizResult
import grammar.CNFConverter._

object JavaGrammar extends Benchmark {
  import GrammarReaders._
  import QuizResult._

  def benchmarkName = "JavaGrammar-simple"
    def benchmarkSource = "Unknown"

  val filename = "javagrammar1"
  
  override def ebnfGrammar = bnfgrammar"""compilation_unit -> ( package_statement )? import_statement* type_declaration*
  field_declaration -> ( ( doc_comment )? ( method_declaration | constructor_declaration | variable_declaration ) ) | static_initializer | ';'
arglist -> expression ( ',' expression )* 
bit_expression -> ( '~' expression ) | ( expression ( '>>=' | '<<' | '>>' | '>>>' ) expression ) 
casting_expression -> '(' type ')' expression 
character -> CHARLIT 
class_declaration -> modifiers 'class' identifier ( 'extends' class_name )? ( 'implements' interface_name other_interfaces )? '{' field_declaration* '}'
modifiers -> modifier*
other_interfaces -> ( ',' interface_name )*
class_name -> identifier | ( package_name '.' identifier ) 
constructor_declaration -> modifiers identifier '(' ( parameter_list )? ')' statement_block 
creating_expression -> 'new' ( ( classe_name '(' ( arglist )? ')' ) | ( type_specifier [ '[' expression ']' ] square_braces ) | ( '(' expression ')' ) ) 
decimal_digits -> DIGITS
doc_comment -> '/**' '...text...' '*/' 
do_statement -> 'do' statement 'while' '(' expression ')' ';' 
exponent_part -> 'e' ( '+' | '-' )? decimal_digits 
expression -> numeric_expression | testing_expression | logical_expression | string_expression | bit_expression | casting_expression | creating_expression | literal_expression | 'null' | 'super' | 'this' | identifier | ( '(' expression ')' ) | ( expression ( ( '(' ( arglist )? ')' ) | ( '[' expression ']' ) | ( '.' expression ) | ( ',' expression ) | ( 'instanceof' ( class_name | interface_name ) ) ) )  
float_literal -> ( decimal_digits '.' ( decimal_digits )? ( exponent_part )? ( float_type_suffix )? ) | ( '.' decimal_digits ( exponent_part )? ( float_type_suffix )? ) | ( decimal_digits ( exponent_part )? ( float_type_suffix )? ) 
float_type_suffix -> 'f' | 'd' 
for_statement -> 'for' '(' ( variable_declaration | ( expression ';' ) | ';' ) ( expression )? ';' ( expression )? ';' ')' statement 
identifier -> IDENTIFIER 
if_statement -> 'if' '(' expression ')' statement ( 'else' statement )? 
import_statement -> 'import' ( ( package_name '.' '*' ';' ) | ( class_name | interface_name ) ) ';' 
integer_literal -> INTLITERAL 
interface_declaration -> modifiers 'interface' identifier ( 'extends' interface_name other_interfaces )? '{' field_declaration* '}' 
interface_name -> identifier | ( package_name '.' identifier ) 
literal_expression -> integer_literal | float_literal | string | character 
logical_expression -> 'true' | 'false' | ( '!' expression ) | ( expression ( 'ampersand' | 'ampersand=' | '|' | '|=' | '^' | '^=' | ( 'ampersand' 'ampersand' ) | '||=' | '%' | '%=' ) expression ) | ( expression '?' expression ':' expression ) 
method_declaration -> modifiers type identifier '(' ( parameter_list )? ')' square_braces ( ';' | statement_block ) 
square_braces -> ( '[' ']' )*
modifier -> 'public' | 'private' | 'protected' | 'static' | 'final' | 'native' | 'synchronized' | 'abstract' | 'threadsafe' | 'transient' 
numeric_expression -> ( ( '-' | '++' | '--' ) expression ) | ( expression ( '++' | '--' ) ) | ( expression ( '+' | '+=' | '-' | '-=' | '*' | '*=' | '/' | '/=' | '%' | '%=' ) expression ) 
package_name -> identifier | ( package_name '.' identifier ) 
package_statement -> 'package' package_name ';' 
parameter -> type identifier square_braces
parameter_list -> parameter ( ',' parameter )* 
statement -> ( ';' ) | variable_declaration | ( expression ';' ) | ( statement_block ) | ( if_statement ) | ( do_statement ) | ( while_statement ) | ( for_statement ) | ( try_statement ) | ( switch_statement ) | ( 'synchronized' '(' expression ')' statement ) | ( 'return' ( expression )? ';' ) | ( 'throw' expression ';' ) | ( identifier ':' statement ) | ( 'break' ( identifier )? ';' ) | ( 'continue' ( identifier )? ';' )  
statement_block -> '{' statement* '}' 
static_initializer -> 'static' statement_block 
string -> " character* " 
string_expression -> ( expression ( '+' | '+=' ) expression ) 
switch_statement -> 'switch' '(' expression ')' '{' ( ( 'case' expression ':' ) | ( 'default' ':' ) | statement )* '}' 
testing_expression -> ( expression ( '>' | '<' | '>=' | '<=' | '==' | '!=' ) expression ) 
try_statement -> 'try' statement ( 'catch' '(' parameter ')' statement )* ( 'finally' statement )? 
type -> type_specifier square_braces 
type_declaration -> ( doc_comment )? ( class_declaration | interface_declaration ) ';' 
type_specifier -> 'boolean' | 'byte' | 'char' | 'short' | 'int' | 'float' | 'long' | 'double' | class_name | interface_name 
variable_declaration -> modifiers type variable_declarator ( ',' variable_declarator )* ';' 
variable_declarator -> identifier square_braces ( '=' variable_initializer )? 
variable_initializer -> expression | ( '{' ( variable_initializer ( ',' variable_initializer )* ( ',' )? )? '}' ) 
while_statement -> 'while' '(' expression ')' statement"""

  /*this add bnfgrammar"""compilation_unit -> ( package_statement )? import_statement* type_declaration*
  field_declaration -> ( ( doc_comment )? ( method_declaration | variable_declaration) ) | static_initializer | ';'
arglist -> expression ( ',' expression )* 
bit_expression -> ( '~' expression ) | ( expression ( '>>=' | '<<' | '>>' | '>>>' ) expression ) 
casting_expression -> '(' type ')' expression 
character -> CHARLIT 
class_declaration -> modifiers 'class' identifier ( 'extends' class_name )? ( 'implements' interface_name other_interfaces )? '{' field_declaration* '}'
modifiers -> modifier*
other_interfaces -> ( ',' interface_name )*
class_name -> identifier | ( package_name '.' identifier ) 
constructor_declaration -> modifiers identifier '(' ( parameter_list )? ')' statement_block 
creating_expression -> 'new' ( ( classe_name '(' ( arglist )? ')' ) | ( type_specifier [ '[' expression ']' ] square_braces ) | ( '(' expression ')' ) ) 
decimal_digits -> DIGITS
doc_comment -> '/**' '...text...' '*/' 
do_statement -> 'do' statement 'while' '(' expression ')' ';' 
exponent_part -> 'e' ( '+' | '-' )? decimal_digits 
expression -> numeric_expression | testing_expression | logical_expression | string_expression | bit_expression | casting_expression | creating_expression | literal_expression | 'null' | 'super' | 'this' | identifier | ( '(' expression ')' ) | ( expression ( ( '(' ( arglist )? ')' ) | ( '[' expression ']' ) | ( '.' expression ) | ( ',' expression ) | ( 'instanceof' ( class_name | interface_name ) ) ) )  
float_literal -> ( decimal_digits '.' ( decimal_digits )? ( exponent_part )? ( float_type_suffix )? ) | ( '.' decimal_digits ( exponent_part )? ( float_type_suffix )? ) | ( decimal_digits ( exponent_part )? ( float_type_suffix )? ) 
float_type_suffix -> 'f' | 'd' 
for_statement -> 'for' '(' ( variable_declaration | ( expression ';' ) | ';' ) ( expression )? ';' ( expression )? ';' ')' statement 
identifier -> IDENTIFIER 
if_statement -> 'if' '(' expression ')' statement ( 'else' statement )? 
import_statement -> 'import' ( ( package_name '.' '*' ';' ) | ( class_name | interface_name ) ) ';' 
integer_literal -> INTLITERAL 
interface_declaration -> modifiers 'interface' identifier ( 'extends' interface_name other_interfaces )? '{' field_declaration* '}' 
interface_name -> identifier | ( package_name '.' identifier ) 
literal_expression -> integer_literal | float_literal | string | character 
logical_expression -> 'true' | 'false' | ( '!' expression ) | ( expression ( 'ampersand' | 'ampersand=' | '|' | '|=' | '^' | '^=' | ( 'ampersand' 'ampersand' ) | '||=' | '%' | '%=' ) expression ) | ( expression '?' expression ':' expression ) 
method_declaration -> modifiers type identifier '(' ( parameter_list )? ')' square_braces ( ';' | statement_block ) 
square_braces -> ( '[' ']' )*
modifier -> 'public' | 'private' | 'protected' | 'static' | 'final' | 'native' | 'synchronized' | 'abstract' | 'threadsafe' | 'transient' 
numeric_expression -> ( ( '-' | '++' | '--' ) expression ) | ( expression ( '++' | '--' ) ) | ( expression ( '+' | '+=' | '-' | '-=' | '*' | '*=' | '/' | '/=' | '%' | '%=' ) expression ) 
package_name -> identifier | ( package_name '.' identifier ) 
package_statement -> 'package' package_name ';' 
parameter -> type identifier square_braces
parameter_list -> parameter ( ',' parameter )* 
statement -> ( ';' ) | variable_declaration | ( expression ';' ) | ( statement_block ) | ( if_statement ) | ( do_statement ) | ( while_statement ) | ( for_statement ) | ( try_statement ) | ( switch_statement ) | ( 'synchronized' '(' expression ')' statement ) | ( 'return' ( expression )? ';' ) | ( 'throw' expression ';' ) | ( identifier ':' statement ) | ( 'break' ( identifier )? ';' ) | ( 'continue' ( identifier )? ';' )  
statement_block -> '{' statement* '}' 
static_initializer -> 'static' statement_block 
string -> " character* " 
string_expression -> ( expression ( '+' | '+=' ) expression ) 
switch_statement -> 'switch' '(' expression ')' '{' ( ( 'case' expression ':' ) | ( 'default' ':' ) | statement )* '}' 
testing_expression -> ( expression ( '>' | '<' | '>=' | '<=' | '==' | '!=' ) expression ) 
try_statement -> 'try' statement ( 'catch' '(' parameter ')' statement )* ( 'finally' statement )? 
type -> type_specifier square_braces 
type_declaration -> ( doc_comment )? ( class_declaration | interface_declaration ) ';' 
type_specifier -> 'boolean' | 'byte' | 'char' | 'short' | 'int' | 'float' | 'long' | 'double' | class_name | interface_name 
variable_declaration -> modifiers type variable_declarator ( ',' variable_declarator )* ';' 
variable_declarator -> identifier square_braces ( '=' variable_initializer )? 
variable_initializer -> expression | ( '{' ( variable_initializer ( ',' variable_initializer )* ( ',' )? )? '}' ) 
while_statement -> 'while' '(' expression ')' statement"""*/
  
  /*def dumpBoundedReference() {
    val g = reference.cfGrammar.fromCNF 
    val plainbg = (new GrammarBoundingHelper(g)).boundGrammar(1)
    //remove unreachable and unproductive non-terminals if any
    val bg = (removeUnreachableRules _ andThen removeUnproductiveRules)(plainbg)
    GrammarWriter.dumpGrammar(filename, bg)
  }*/

  /*def dumpBoundedErrorGrammar() {    
    val g = this.student_grammars(0).grammar.cfGrammar.fromCNF     
    val plainbg = (new GrammarBoundingHelper(g)).boundGrammar(1)
    //remove unreachable and unproductive non-terminals if any
    val bg = (removeUnreachableRules _ andThen removeUnproductiveRules)(plainbg)
    GrammarWriter.dumpGrammar(filename+"-error", bg)
  }*/
}