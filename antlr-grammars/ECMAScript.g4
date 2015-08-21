program
 : sourceElements?
 ;

sourceElements
 : sourceElement+
 ;

sourceElement
 : statement
 | functionDeclaration
 ;

statement
 : block
 | variableStatement
 | emptyStatement
 | expressionStatement
 | ifStatement
 | iterationStatement
 | continueStatement
 | breakStatement
 | returnStatement
 | withStatement
 | labelledStatement
 | switchStatement
 | throwStatement
 | tryStatement
 ;

block
 : '{' statementList? '}'
 ;

statementList
 : statement+
 ;

variableStatement
 : Var variableDeclarationList 
 ;

variableDeclarationList
 : variableDeclaration ( ',' variableDeclaration )*
 ;

variableDeclaration
 : Identifier initialiser?
 ;

initialiser
 : '=' singleExpression
 ;

emptyStatement
 : SemiColon
 ;

expressionStatement
 : expressionSequence
 ;

ifStatement
 : If '(' expressionSequence ')' statement ( Else statement )?
 ;

iterationStatement
 : Do statement While '(' expressionSequence ')'                                                  # DoStatement
 | While '(' expressionSequence ')' statement                                                        # WhileStatement
 | For '(' expressionSequence? ';' expressionSequence? ';' expressionSequence? ')' statement         # ForStatement
 | For '(' Var variableDeclarationList ';' expressionSequence? ';' expressionSequence? ')' statement # ForVarStatement
 | For '(' singleExpression In expressionSequence ')' statement                                      # ForInStatement
 | For '(' Var variableDeclaration In expressionSequence ')' statement                               # ForVarInStatement
 ;

continueStatement
 : Continue Identifier? 
 ;

breakStatement
 : Break Identifier? 
 ;

returnStatement
 : Return expressionSequence? 
 ;

withStatement
 : With '(' expressionSequence ')' statement
 ;

switchStatement
 : Switch '(' expressionSequence ')' caseBlock
 ;

caseBlock
 : '{' caseClauses? ( defaultClause caseClauses? )? '}'
 ;

caseClauses
 : caseClause+
 ;

caseClause
 : Case expressionSequence ':' statementList?
 ;

defaultClause
 : Default ':' statementList?
 ;

labelledStatement
 : Identifier ':' statement
 ;

throwStatement
 : Throw expressionSequence 
 ;

tryStatement
 : Try block catchProduction
 | Try block finallyProduction
 | Try block catchProduction finallyProduction
 ;

catchProduction
 : Catch '(' Identifier ')' block
 ;

finallyProduction
 : Finally block
 ;


functionDeclaration
 : Function Identifier '(' formalParameterList? ')' '{' functionBody '}'
 ;

formalParameterList
 : Identifier ( ',' Identifier )*
 ;

functionBody
 : sourceElements?
 ;
    
arrayLiteral
 : '[' elementList? ','? elision? ']'
 ;

/// ElementList :
///     Elision? AssignmentExpression
///     ElementList , Elision? AssignmentExpression
elementList
 : elision? singleExpression ( ',' elision? singleExpression )*
 ;

/// Elision :
///     ,
///     Elision ,
elision
 : ','+
 ;

/// ObjectLiteral :
///     { }
///     { PropertyNameAndValueList }
///     { PropertyNameAndValueList , }
objectLiteral
 : '{' propertyNameAndValueList? ','? '}'
 ;

/// PropertyNameAndValueList :
///     PropertyAssignment
///     PropertyNameAndValueList , PropertyAssignment
propertyNameAndValueList
 : propertyAssignment ( ',' propertyAssignment )*
 ;
    
/// PropertyAssignment :
///     PropertyName : AssignmentExpression
///     get PropertyName ( ) { FunctionBody }
///     set PropertyName ( PropertySetParameterList ) { FunctionBody }
propertyAssignment
 : propertyName ':' singleExpression                            # PropertyExpressionAssignment
 | getter '(' ')' '{' functionBody '}'                          # PropertyGetter
 | setter '(' propertySetParameterList ')' '{' functionBody '}' # PropertySetter
 ;           
    
/// PropertyName :
///     IdentifierName
///     StringLiteral
///     NumericLiteral
propertyName
 : identifierName
 | StringLiteral
 | numericLiteral
 ;
    
/// PropertySetParameterList :
///     Identifier
propertySetParameterList
 : Identifier
 ;

/// Arguments :
///     ( )
///     ( ArgumentList )
arguments
 : '(' argumentList? ')'
 ;
    
argumentList
 : singleExpression ( ',' singleExpression )*
 ;
    
expressionSequence
 : singleExpression ( ',' singleExpression )*
 ;

singleExpression
 : Function Identifier? '(' formalParameterList? ')' '{' functionBody '}' 
 | singleExpression '[' expressionSequence ']'                            
 | singleExpression '.' identifierName                                    
 | singleExpression arguments                                             
 | New singleExpression arguments?                                        
 | singleExpression '++'                         
 | singleExpression '--'                         
 | Delete singleExpression                                                
 | Void singleExpression                                                  
 | Typeof singleExpression                                                
 | '++' singleExpression                                                  
 | '--' singleExpression                                                  
 | '+' singleExpression                                                   
 | '-' singleExpression                                                   
 | '~' singleExpression                                                   
 | '!' singleExpression                                                   
 | singleExpression ( '*' | '/' | '%' ) singleExpression                  
 | singleExpression ( '+' | '-' ) singleExpression                        
 | singleExpression ( '<<' | '>>' | '>>>' ) singleExpression              
 | singleExpression ( '<' | '>' | '<=' | '>=' ) singleExpression          
 | singleExpression Instanceof singleExpression                           
 | singleExpression In singleExpression                                   
 | singleExpression ( '==' | '!=' | '===' | '!==' ) singleExpression      
 | singleExpression '&' singleExpression                                  
 | singleExpression '^' singleExpression                                  
 | singleExpression '|' singleExpression                                  
 | singleExpression '&&' singleExpression                                 
 | singleExpression '||' singleExpression                                 
 | singleExpression '?' singleExpression ':' singleExpression             
 | singleExpression '=' expressionSequence                                
 | singleExpression assignmentOperator expressionSequence                 
 | This                                                                   
 | Identifier                                                             
 | literal                                                                
 | arrayLiteral                                                           
 | objectLiteral                                                          
 | '(' expressionSequence ')'                                             
 ;

/// AssignmentOperator : one of
///     *=	/=	%=	+=	-=	<<=	>>=	>>>=	&=	^=	|=
assignmentOperator
 : '*=' 
 | '/=' 
 | '%=' 
 | '+=' 
 | '-=' 
 | '<<=' 
 | '>>=' 
 | '>>>=' 
 | '&=' 
 | '^=' 
 | '|='
 ;

literal
 : ( NullLiteral 
   | BooleanLiteral
   | StringLiteral
   | RegularExpression
   )
 | numericLiteral
 ;

numericLiteral
 : Number
 ;

identifierName
 : Identifier
 | reservedWord
 ;

reservedWord
 : keyword
 | ( NullLiteral
   | BooleanLiteral
   )
 ;

keyword
 : Break
 | Do
 | Instanceof
 | Typeof
 | Case
 | Else
 | New
 | Var
 | Catch
 | Finally
 | Return
 | Void
 | Continue
 | For
 | Switch
 | While
 | Function
 | This
 | With
 | Default
 | If
 | Throw
 | Delete
 | In
 | Try
 ;

getter
 : Identifier
 ;

setter
 : Identifier
 ;

/// 7.8.1 Null Literals
NullLiteral
 : 'null'
 ;

/// 7.8.2 Boolean Literals
BooleanLiteral
 : 'true'
 | 'false'
 ;

/// 7.6.1.1 Keywords
Break      : 'break'
	   ;
Do         : 'do'
	  ;
Instanceof : 'instanceof'
;
Typeof     : 'typeof'
;
Case       : 'case'
;
Else       : 'else'
;
New        : 'new'
;
Var        : 'var'
;
Catch      : 'catch'
;
Finally    : 'finally'
;
Return     : 'return'
;
Void       : 'void'
;
Continue   : 'continue'
;
For        : 'for'
;
Switch     : 'switch'
;
While      : 'while'
;
Function   : 'function'
;
This       : 'this'
;
With       : 'with'
;
Default    : 'default'
;
If         : 'if'
;
Throw      : 'throw'
;
Delete     : 'delete'
;
In         : 'in'
;
Try        : 'try'
;

/// 7.8.4 String Literals
StringLiteral
 : String
 ;
