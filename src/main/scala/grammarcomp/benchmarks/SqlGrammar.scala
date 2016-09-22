package grammarcomp

package benchmarks
import grammar._
import grammar.CFGrammar._
import grammar.GrammarReaders.GrammarImplicit
import grammar.examples.QuizResult

/* From https://github.com/antlr/grammars-v4/blob/master/sqlite/SQLite.g4 */

object SqlGrammar extends QuizResult {
  val note = """ Used the following regular expressions for replacement in notepad++:
"\r?\n : "
" -> "

"^([A-Z_0-9]+) : (.*);$"
"$1 -> $2"


" ;\r?\n\r?\n"
""

"\r?\n +"
" "

"^/\*((?!\*/)(.|\r|\n))*\*/\r?\n"
""

"^//.*\r?\n"
""

"//.*"
""

"fragment ([A-Z]) : \[(\w)(\w)\];"
"$1 -> "$2" | "$3""

"fragment DIGIT : \[0-9\];\r?\n"
""

" -> channel\(HIDDEN\)$"
""
"""

  import GrammarReaders._
  import QuizResult._
  
  implicit def MAX_GRADE = 2
  
  override def quizName = "SqlGrammar"
  
  override def reference = bnfgrammar"""parse -> ( sql_stmt_list | error )* EOF
error -> UNEXPECTED_CHAR
sql_stmt_list -> ';'* sql_stmt ( ';'+ sql_stmt )* ';'*
sql_stmt -> ( K_EXPLAIN ( K_QUERY K_PLAN )? )? ( alter_table_stmt | analyze_stmt | attach_stmt | begin_stmt | commit_stmt | compound_select_stmt | create_index_stmt | create_table_stmt | create_trigger_stmt | create_view_stmt | create_virtual_table_stmt | delete_stmt | delete_stmt_limited | detach_stmt | drop_index_stmt | drop_table_stmt | drop_trigger_stmt | drop_view_stmt | factored_select_stmt | insert_stmt | pragma_stmt | reindex_stmt | release_stmt | rollback_stmt | savepoint_stmt | simple_select_stmt | select_stmt | update_stmt | update_stmt_limited | vacuum_stmt )
alter_table_stmt -> K_ALTER K_TABLE ( database_name '.' )? table_name ( K_RENAME K_TO new_table_name | K_ADD K_COLUMN? column_def )
analyze_stmt -> K_ANALYZE ( database_name | table_or_index_name | database_name '.' table_or_index_name )?
attach_stmt -> K_ATTACH K_DATABASE? expr K_AS database_name
begin_stmt -> K_BEGIN ( K_DEFERRED | K_IMMEDIATE | K_EXCLUSIVE )? ( K_TRANSACTION transaction_name? )?
commit_stmt -> ( K_COMMIT | K_END ) ( K_TRANSACTION transaction_name? )?
compound_select_stmt -> ( K_WITH K_RECURSIVE? common_table_expression ( ',' common_table_expression )* )? select_core ( ( K_UNION K_ALL? | K_INTERSECT | K_EXCEPT ) select_core )+ ( K_ORDER K_BY ordering_term ( ',' ordering_term )* )? ( K_LIMIT expr ( ( K_OFFSET | ',' ) expr )? )?
create_index_stmt -> K_CREATE K_UNIQUE? K_INDEX ( K_IF K_NOT K_EXISTS )? ( database_name '.' )? index_name K_ON table_name '(' indexed_column ( ',' indexed_column )* ')' ( K_WHERE expr )?
create_table_stmt -> K_CREATE ( K_TEMP | K_TEMPORARY )? K_TABLE ( K_IF K_NOT K_EXISTS )? ( database_name '.' )? table_name ( '(' column_def ( ',' column_def )* ( ',' table_constraint )* ')' ( K_WITHOUT IDENTIFIER )? | K_AS select_stmt  )
create_trigger_stmt -> K_CREATE ( K_TEMP | K_TEMPORARY )? K_TRIGGER ( K_IF K_NOT K_EXISTS )? ( database_name '.' )? trigger_name ( K_BEFORE  | K_AFTER | K_INSTEAD K_OF )?  ( K_DELETE | K_INSERT | K_UPDATE ( K_OF column_name ( ',' column_name )* )? ) K_ON ( database_name '.' )? table_name ( K_FOR K_EACH K_ROW )? ( K_WHEN expr )? K_BEGIN ( ( update_stmt | insert_stmt | delete_stmt | select_stmt ) ';' )+ K_END
create_view_stmt -> K_CREATE ( K_TEMP | K_TEMPORARY )? K_VIEW ( K_IF K_NOT K_EXISTS )? ( database_name '.' )? view_name K_AS select_stmt
create_virtual_table_stmt -> K_CREATE K_VIRTUAL K_TABLE ( K_IF K_NOT K_EXISTS )? ( database_name '.' )? table_name K_USING module_name ( '(' module_argument ( ',' module_argument )* ')' )?
delete_stmt -> with_clause? K_DELETE K_FROM qualified_table_name  ( K_WHERE expr )?
delete_stmt_limited -> with_clause? K_DELETE K_FROM qualified_table_name  ( K_WHERE expr )? ( ( K_ORDER K_BY ordering_term ( ',' ordering_term )* )? K_LIMIT expr ( ( K_OFFSET | ',' ) expr )? )?
detach_stmt -> K_DETACH K_DATABASE? database_name
drop_index_stmt -> K_DROP K_INDEX ( K_IF K_EXISTS )? ( database_name '.' )? index_name
drop_table_stmt -> K_DROP K_TABLE ( K_IF K_EXISTS )? ( database_name '.' )? table_name
drop_trigger_stmt -> K_DROP K_TRIGGER ( K_IF K_EXISTS )? ( database_name '.' )? trigger_name
drop_view_stmt -> K_DROP K_VIEW ( K_IF K_EXISTS )? ( database_name '.' )? view_name
factored_select_stmt -> ( K_WITH K_RECURSIVE? common_table_expression ( ',' common_table_expression )* )? select_core ( compound_operator select_core )* ( K_ORDER K_BY ordering_term ( ',' ordering_term )* )? ( K_LIMIT expr ( ( K_OFFSET | ',' ) expr )? )?
insert_stmt -> with_clause? ( K_INSERT  | K_REPLACE | K_INSERT K_OR K_REPLACE | K_INSERT K_OR K_ROLLBACK | K_INSERT K_OR K_ABORT | K_INSERT K_OR K_FAIL | K_INSERT K_OR K_IGNORE ) K_INTO ( database_name '.' )? table_name ( '(' column_name ( ',' column_name )* ')' )? ( K_VALUES '(' expr ( ',' expr )* ')' ( ',' '(' expr ( ',' expr )* ')' )* | select_stmt | K_DEFAULT K_VALUES )
pragma_stmt -> K_PRAGMA ( database_name '.' )? pragma_name ( '=' pragma_value | '(' pragma_value ')' )?
reindex_stmt -> K_REINDEX ( collation_name | ( database_name '.' )? ( table_name | index_name ) )?
release_stmt -> K_RELEASE K_SAVEPOINT? savepoint_name
rollback_stmt -> K_ROLLBACK ( K_TRANSACTION transaction_name? )? ( K_TO K_SAVEPOINT? savepoint_name )?
savepoint_stmt -> K_SAVEPOINT savepoint_name
simple_select_stmt -> ( K_WITH K_RECURSIVE? common_table_expression ( ',' common_table_expression )* )? select_core ( K_ORDER K_BY ordering_term ( ',' ordering_term )* )? ( K_LIMIT expr ( ( K_OFFSET | ',' ) expr )? )?
select_stmt -> ( K_WITH K_RECURSIVE? common_table_expression ( ',' common_table_expression )* )? select_or_values ( compound_operator select_or_values )* ( K_ORDER K_BY ordering_term ( ',' ordering_term )* )? ( K_LIMIT expr ( ( K_OFFSET | ',' ) expr )? )?
select_or_values -> K_SELECT ( K_DISTINCT | K_ALL )? result_column ( ',' result_column )* ( K_FROM ( table_or_subquery ( ',' table_or_subquery )* | join_clause ) )? ( K_WHERE expr )? ( K_GROUP K_BY expr ( ',' expr )* ( K_HAVING expr )? )? | K_VALUES '(' expr ( ',' expr )* ')' ( ',' '(' expr ( ',' expr )* ')' )*
update_stmt -> with_clause? K_UPDATE ( K_OR K_ROLLBACK | K_OR K_ABORT | K_OR K_REPLACE | K_OR K_FAIL | K_OR K_IGNORE )? qualified_table_name K_SET column_name '=' expr ( ',' column_name '=' expr )* ( K_WHERE expr )?
update_stmt_limited -> with_clause? K_UPDATE ( K_OR K_ROLLBACK | K_OR K_ABORT | K_OR K_REPLACE | K_OR K_FAIL | K_OR K_IGNORE )? qualified_table_name K_SET column_name '=' expr ( ',' column_name '=' expr )* ( K_WHERE expr )? ( ( K_ORDER K_BY ordering_term ( ',' ordering_term )* )? K_LIMIT expr ( ( K_OFFSET | ',' ) expr )?  )?
vacuum_stmt -> K_VACUUM
column_def -> column_name type_name? column_constraint*
type_name -> name+ ( '(' signed_number ')' | '(' signed_number ',' signed_number ')' )?
column_constraint -> ( K_CONSTRAINT name )? ( K_PRIMARY K_KEY ( K_ASC | K_DESC )? conflict_clause K_AUTOINCREMENT? | K_NOT? K_NULL conflict_clause | K_UNIQUE conflict_clause | K_CHECK '(' expr ')' | K_DEFAULT (signed_number | literal_value | '(' expr ')') | K_COLLATE collation_name | foreign_key_clause )
conflict_clause -> ( K_ON K_CONFLICT ( K_ROLLBACK | K_ABORT | K_FAIL | K_IGNORE | K_REPLACE ) )?
expr -> literal_value | BIND_PARAMETER | ( ( database_name '.' )? table_name '.' )? column_name | unary_operator expr | expr '||' expr | expr ( '*' | '/' | '%' ) expr | expr ( '+' | '-' ) expr | expr ( '<<' | '>>' | '&' | '|' ) expr | expr ( '<' | '<=' | '>' | '>=' ) expr | expr ( '=' | '==' | '!=' | '<>' | K_IS | K_IS K_NOT | K_IN | K_LIKE | K_GLOB | K_MATCH | K_REGEXP ) expr | expr K_AND expr | expr K_OR expr | function_name '(' ( K_DISTINCT? expr ( ',' expr )* | '*' )? ')' | '(' expr ')' | K_CAST '(' expr K_AS type_name ')' | expr K_COLLATE collation_name | expr K_NOT? ( K_LIKE | K_GLOB | K_REGEXP | K_MATCH ) expr ( K_ESCAPE expr )? | expr ( K_ISNULL | K_NOTNULL | K_NOT K_NULL ) | expr K_IS K_NOT? expr | expr K_NOT? K_BETWEEN expr K_AND expr | expr K_NOT? K_IN ( '(' ( select_stmt | expr ( ',' expr )* )?  ')' | ( database_name '.' )? table_name ) | ( ( K_NOT )? K_EXISTS )? '(' select_stmt ')' | K_CASE expr? ( K_WHEN expr K_THEN expr )+ ( K_ELSE expr )? K_END | raise_function
foreign_key_clause -> K_REFERENCES foreign_table ( '(' column_name ( ',' column_name )* ')' )? ( ( K_ON ( K_DELETE | K_UPDATE ) ( K_SET K_NULL | K_SET K_DEFAULT | K_CASCADE | K_RESTRICT | K_NO K_ACTION ) | K_MATCH name )  )* ( K_NOT? K_DEFERRABLE ( K_INITIALLY K_DEFERRED | K_INITIALLY K_IMMEDIATE )? )?
raise_function -> K_RAISE '(' ( K_IGNORE  | ( K_ROLLBACK | K_ABORT | K_FAIL ) ',' error_message ) ')'
indexed_column -> column_name ( K_COLLATE collation_name )? ( K_ASC | K_DESC )?
table_constraint -> ( K_CONSTRAINT name )? ( ( K_PRIMARY K_KEY | K_UNIQUE ) '(' indexed_column ( ',' indexed_column )* ')' conflict_clause | K_CHECK '(' expr ')' | K_FOREIGN K_KEY '(' column_name ( ',' column_name )* ')' foreign_key_clause )
with_clause -> K_WITH K_RECURSIVE? cte_table_name K_AS '(' select_stmt ')' ( ',' cte_table_name K_AS '(' select_stmt ')' )*
qualified_table_name -> ( database_name '.' )? table_name ( K_INDEXED K_BY index_name | K_NOT K_INDEXED )?
ordering_term -> expr ( K_COLLATE collation_name )? ( K_ASC | K_DESC )?
pragma_value -> signed_number | name | STRING_LITERAL
common_table_expression -> table_name ( '(' column_name ( ',' column_name )* ')' )? K_AS '(' select_stmt ')'
result_column -> '*' | table_name '.' '*' | expr ( K_AS? column_alias )?
table_or_subquery -> ( database_name '.' )? table_name ( K_AS? table_alias )? ( K_INDEXED K_BY index_name | K_NOT K_INDEXED )? | '(' ( table_or_subquery ( ',' table_or_subquery )* | join_clause ) ')' ( K_AS? table_alias )? | '(' select_stmt ')' ( K_AS? table_alias )?
join_clause -> table_or_subquery ( join_operator table_or_subquery join_constraint )*
join_operator -> ',' | K_NATURAL? ( K_LEFT K_OUTER? | K_INNER | K_CROSS )? K_JOIN
join_constraint -> ( K_ON expr | K_USING '(' column_name ( ',' column_name )* ')' )?
select_core -> K_SELECT ( K_DISTINCT | K_ALL )? result_column ( ',' result_column )* ( K_FROM ( table_or_subquery ( ',' table_or_subquery )* | join_clause ) )? ( K_WHERE expr )? ( K_GROUP K_BY expr ( ',' expr )* ( K_HAVING expr )? )? | K_VALUES '(' expr ( ',' expr )* ')' ( ',' '(' expr ( ',' expr )* ')' )*
compound_operator -> K_UNION | K_UNION K_ALL | K_INTERSECT | K_EXCEPT
cte_table_name -> table_name ( '(' column_name ( ',' column_name )* ')' )?
signed_number -> ( '+' | '-' )? NUMERIC_LITERAL
literal_value -> NUMERIC_LITERAL | STRING_LITERAL | BLOB_LITERAL | K_NULL | K_CURRENT_TIME | K_CURRENT_DATE | K_CURRENT_TIMESTAMP
unary_operator -> '-' | '+' | '~' | K_NOT
error_message -> STRING_LITERAL module_argument 
column_alias -> IDENTIFIER | STRING_LITERAL
keyword -> K_ABORT | K_ACTION | K_ADD | K_AFTER | K_ALL | K_ALTER | K_ANALYZE | K_AND | K_AS | K_ASC | K_ATTACH | K_AUTOINCREMENT | K_BEFORE | K_BEGIN | K_BETWEEN | K_BY | K_CASCADE | K_CASE | K_CAST | K_CHECK | K_COLLATE | K_COLUMN | K_COMMIT | K_CONFLICT | K_CONSTRAINT | K_CREATE | K_CROSS | K_CURRENT_DATE | K_CURRENT_TIME | K_CURRENT_TIMESTAMP | K_DATABASE | K_DEFAULT | K_DEFERRABLE | K_DEFERRED | K_DELETE | K_DESC | K_DETACH | K_DISTINCT | K_DROP | K_EACH | K_ELSE | K_END | K_ESCAPE | K_EXCEPT | K_EXCLUSIVE | K_EXISTS | K_EXPLAIN | K_FAIL | K_FOR | K_FOREIGN | K_FROM | K_FULL | K_GLOB | K_GROUP | K_HAVING | K_IF | K_IGNORE | K_IMMEDIATE | K_IN | K_INDEX | K_INDEXED | K_INITIALLY | K_INNER | K_INSERT | K_INSTEAD | K_INTERSECT | K_INTO | K_IS | K_ISNULL | K_JOIN | K_KEY | K_LEFT | K_LIKE | K_LIMIT | K_MATCH | K_NATURAL | K_NO | K_NOT | K_NOTNULL | K_NULL | K_OF | K_OFFSET | K_ON | K_OR | K_ORDER | K_OUTER | K_PLAN | K_PRAGMA | K_PRIMARY | K_QUERY | K_RAISE | K_RECURSIVE | K_REFERENCES | K_REGEXP | K_REINDEX | K_RELEASE | K_RENAME | K_REPLACE | K_RESTRICT | K_RIGHT | K_ROLLBACK | K_ROW | K_SAVEPOINT | K_SELECT | K_SET | K_TABLE | K_TEMP | K_TEMPORARY | K_THEN | K_TO | K_TRANSACTION | K_TRIGGER | K_UNION | K_UNIQUE | K_UPDATE | K_USING | K_VACUUM | K_VALUES | K_VIEW | K_VIRTUAL | K_WHEN | K_WHERE | K_WITH | K_WITHOUT
name -> any_name
function_name -> any_name
database_name -> any_name
table_name  -> any_name
table_or_index_name  -> any_name
new_table_name  -> any_name
column_name  -> any_name
collation_name  -> any_name
foreign_table  -> any_name
index_name  -> any_name
trigger_name -> any_name
view_name  -> any_name
module_name  -> any_name
pragma_name  -> any_name
savepoint_name  -> any_name
table_alias  -> any_name
transaction_name -> any_name
any_name -> IDENTIFIER  | keyword | STRING_LITERAL | '(' any_name ')'
SCOL -> ';'
DOT -> '.'
OPEN_PAR -> '('
CLOSE_PAR -> ')'
COMMA -> ','
ASSIGN -> '='
STAR -> '*'
PLUS -> '+'
MINUS -> '-'
TILDE -> '~'
PIPE2 -> '||'
DIV -> '/'
MOD -> '%'
LT2 -> '<<'
GT2 -> '>>'
AMP -> '&'
PIPE -> '|'
LT -> '<'
LT_EQ -> '<='
GT -> '>'
GT_EQ -> '>='
EQ -> '=='
NOT_EQ1 -> '!='
NOT_EQ2 -> '<>'

K_ABORT -> "abort"
K_ACTION -> "action"
K_ADD -> "add"
K_AFTER -> "after"
K_ALL -> "all"
K_ALTER -> "alter"
K_ANALYZE -> "analyze"
K_AND -> "and"
K_AS -> "as"
K_ASC -> "asc"
K_ATTACH -> "attach"
K_AUTOINCREMENT -> "autoincrement"
K_BEFORE -> "before"
K_BEGIN -> "begin"
K_BETWEEN -> "between"
K_BY -> "by"
K_CASCADE -> "cascade"
K_CASE -> "case"
K_CAST -> "cast"
K_CHECK -> "check"
K_COLLATE -> "collate"
K_COLUMN -> "column"
K_COMMIT -> "commit"
K_CONFLICT -> "conflict"
K_CONSTRAINT -> "constraint"
K_CREATE -> "create"
K_CROSS -> "cross"
K_CURRENT_DATE -> "current_date"
K_CURRENT_TIME -> "current_time"
K_CURRENT_TIMESTAMP -> "current_timestamp"
K_DATABASE -> "database"
K_DEFAULT -> "default"
K_DEFERRABLE -> "deferrable"
K_DEFERRED -> "deferred"
K_DELETE -> "delete"
K_DESC -> "desc"
K_DETACH -> "detach"
K_DISTINCT -> "distinct"
K_DROP -> "drop"
K_EACH -> "each"
K_ELSE -> "else"
K_END -> "end"
K_ESCAPE -> "escape"
K_EXCEPT -> "except"
K_EXCLUSIVE -> "exclusive"
K_EXISTS -> "exists"
K_EXPLAIN -> "explain"
K_FAIL -> "fail"
K_FOR -> "for"
K_FOREIGN -> "foreign"
K_FROM -> "from"
K_FULL -> "full"
K_GLOB -> "glob"
K_GROUP -> "group"
K_HAVING -> "having"
K_IF -> "if"
K_IGNORE -> "ignore"
K_IMMEDIATE -> "immediate"
K_IN -> "in"
K_INDEX -> "index"
K_INDEXED -> "indexed"
K_INITIALLY -> "initially"
K_INNER -> "inner"
K_INSERT -> "insert"
K_INSTEAD -> "instead"
K_INTERSECT -> "intersect"
K_INTO -> "into"
K_IS -> "is"
K_ISNULL -> "isnull"
K_JOIN -> "join"
K_KEY -> "key"
K_LEFT -> "left"
K_LIKE -> "like"
K_LIMIT -> "limit"
K_MATCH -> "match"
K_NATURAL -> "natural"
K_NO -> "no"
K_NOT -> "not"
K_NOTNULL -> "notnull"
K_NULL -> "null"
K_OF -> "of"
K_OFFSET -> "offset"
K_ON -> "on"
K_OR -> "or"
K_ORDER -> "order"
K_OUTER -> "outer"
K_PLAN -> "plan"
K_PRAGMA -> "pragma"
K_PRIMARY -> "primary"
K_QUERY -> "query"
K_RAISE -> "raise"
K_RECURSIVE -> "recursive"
K_REFERENCES -> "references"
K_REGEXP -> "regexp"
K_REINDEX -> "reindex"
K_RELEASE -> "release"
K_RENAME -> "rename"
K_REPLACE -> "replace"
K_RESTRICT -> "restrict"
K_RIGHT -> "right"
K_ROLLBACK -> "rollback"
K_ROW -> "row"
K_SAVEPOINT -> "savepoint"
K_SELECT -> "select"
K_SET -> "set"
K_TABLE -> "table"
K_TEMP -> "temp"
K_TEMPORARY -> "temporary"
K_THEN -> "then"
K_TO -> "to"
K_TRANSACTION -> "transaction"
K_TRIGGER -> "trigger"
K_UNION -> "union"
K_UNIQUE -> "unique"
K_UPDATE -> "update"
K_USING -> "using"
K_VACUUM -> "vacuum"
K_VALUES -> "values"
K_VIEW -> "view"
K_VIRTUAL -> "virtual"
K_WHEN -> "when"
K_WHERE -> "where"
K_WITH -> "with"
K_WITHOUT -> "without"

A -> "a" | "A"
B -> "b" | "B"
C -> "c" | "C"
D -> "d" | "D"
E -> "e" | "E"
F -> "f" | "F"
G -> "g" | "G"
H -> "h" | "H"
I -> "i" | "I"
J -> "j" | "J"
K -> "k" | "K"
L -> "l" | "L"
M -> "m" | "M"
N -> "n" | "N"
O -> "o" | "O"
P -> "p" | "P"
Q -> "q" | "Q"
R -> "r" | "R"
S -> "s" | "S"
T -> "t" | "T"
U -> "u" | "U"
V -> "v" | "V"
W -> "w" | "W"
X -> "x" | "X"
Y -> "y" | "Y"
Z -> "z" | "Z""""
}