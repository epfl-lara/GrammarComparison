package grammarcomp

package grammar.examples
import grammar._
import CFGrammar._
import GrammarReaders.GrammarImplicit

object Quiz2009 extends QuizResult {
  import GrammarReaders._
  import QuizResult._
  
  implicit def MAX_GRADE = 7
  
  override def quizName = "Quiz2009"
    
  override def reference = bnfgrammar"""ex -> ex '+' ex | ex '*' ex | ex ex | ex / ex | '(' ex ')' | ID | INTLITERAL"""
  // The task is to make this grammar LL(1) with the correct priorities
  
/*  this add bnfgrammar"""ex -> ex7
  ex6 -> '(' ex7 ')'
  ex7 -> ex8 ('+' ex7)*  
  ex8 -> ex9 ((('*' | /)? (ex6 | ex8))*)    
  ex9 -> ID | INTLITERAL""" grade 5 comment "More complicated than it needs to be"
  
  this add bnfgrammar"""ex -> ex2
  ex2 -> ex3 plusex2* | '(' ex2 ')'
  plusex2 -> '+' ex2  
  ex3 -> ex4 (('*' | /)? ex3)*  | '(' ex3 ')'    
  ex4 -> ID | INTLITERAL""" grade 5 comment "Conflict (ex2) (ex3), one of them is useless"
  
  this add bnfgrammar"""ex -> factor plusfactor*
  plusfactor ->  '+' factor 
  factor -> value ('*' value | value | / value)*    
  value -> '(' ex ')' | ID | INTLITERAL""" is correct
  
  this add bnfgrammar"""ex -> mex '+' ex | mex
  mex -> ex2 '*' mex | ex2 / mex | ex2 mex | ex2
  ex2 -> '(' ex ')' | ID | INTLITERAL""" grade 5 comment "underlined ex2 mex and ex2"  
  
  this add bnfgrammar"""E -> P | M | L
  P -> M '+' M | M '+' P | '(' E ')'
  M -> L '*' L | L L | L / L | L '*' M | L M | M / L | '(' E ')' | L
  L -> ID | INTLITERAL | '(' E ')'""" grade 3 comment "useless ( E ) two times and still ambiguous"  
  
  this add bnfgrammar"""ex -> f '+' ex | f
  f -> s '*' f | s f | s / f | s
  s -> '(' ex ')' | ID | INTLITERAL""" grade 5 comment "underlined s in 's / f' and in '| s'"
  
  this add bnfgrammar"""ex -> ex1 | '(' ex1 ')'
  ex1 -> ex2 ('+' ex1)*
  ex2 -> ex3 (('*' | / | "" ) ex2)*
  ex3 -> ID | INTLITERAL""" grade 5 comment "((ex1)) would not parse"
  
  this add bnfgrammar"""S0 -> ex1
  ex1 -> ex2 '+' ex1 | ex2 | '(' ex1 ')'
  ex2 -> term / term | term '*' term | term term | term
  term -> ID | INTLITERAL""" grade 4 comment "INTLITERAL * ( ID ) cannot be parsed"
  
  this add bnfgrammar"""ex -> A ex1
  ex1 -> '+' A ex1
  ex1 -> 
  A -> B A1
  A1 -> '*' B A1
  A1 -> / B A1
  A1 -> B A1
  A1 -> 
  B -> ID | INTLITERAL
  B -> '(' ex ')'""" grade 7
  
  this add bnfgrammar"""exAdd -> exFact '+' exAdd | exFact
  exFact -> ex exFact |  ex '*' exFact | ex / exFact | ex
  ex -> '(' exAdd ')' | ID | INTLITERAL""" grade 5 comment "ex / exFact and ex are ambiguous"
  
  this add bnfgrammar"""ex -> prodExpr '+' ex | prodExpr
  prodExpr -> expr (('*' | / |)? prodExpr)*  
  expr -> ID | INTLITERAL | '(' ex ')'""" grade 7

  this add bnfgrammar"""ex4 -> ex3 ('+' ex4)*  
  ex3 -> ex2 (('*' | /)? ex3)*
  ex2 -> ex1 | '(' ex2 ')'
  ex1 -> ID | INTLITERAL""" grade 7
  
  this add bnfgrammar"""ex -> ex '*' ex
  ex -> ex / ex
  ex -> ex '+' ex
  ex -> '(' ex ')'
  ex -> var
  var -> ID INTLITERAL | ID | INTLITERAL""" grade 2 comment "Left recursion on 1) and 2), still ambiguous, on top of not allowing strings of the form 5x"
  
  this add bnfgrammar"""ex -> '(' ex7 ')' | ex7
  ex7 -> ex8 ('+' ex7)*  
  ex8 -> ex9 (('*' | /)? ex9)* | ex9 ex8    
  ex9 -> ID | INTLITERAL""" grade 5 comment "1) What about nested parentheses?"
  
  this add bnfgrammar"""ex2 -> ex ('+' ex1)*  
  ex1 -> ex0 (('*' | /)? ex1)*   
  ex0 -> ID | INTLITERAL""" grade 2 comment "parentheses? ex ex ?"
  
  this add bnfgrammar"""p1 -> p2 ('+' p1)*  
  p2 -> p3 (('*' | /)? p2)*    
  p3 -> '(' p1 ')' | p4
  p4 -> ID | INTLITERAL""" grade 7
  
  this add bnfgrammar"""exp -> A exp1
  exp1 -> '+' A exp1 | 
  A -> B A1
  A1 -> '*' B A1 | B A1 |
  B -> '(' exp ')' | B1
  B1 -> ID | INTLITERAL""" grade 6 comment "2) should add '+' exp | nothing, 4) should add * A | A | nothing, 6) could be in 5). Where did / go? "
  
  this add bnfgrammar"""ex1 -> '(' ex2 ')' | ex2
  ex2 -> ex3 ('+' ex2)*
  opplusex2 -> |  opplusex2
  ex3 -> ex4 (('*' | /)? ex3)*  
  ex4 -> ID | INTLITERAL""" grade 5 comment "((2)) would not parse. This rule should be in ex4. Many expressions with () later on, like 3 + (2 + 1)"
  
  this add bnfgrammar"""ex -> addEx
  addEx -> mulEx ('+' addEx)*  
  mulEx -> sEx (('*' | /)? mulEx)*
  sEx -> '(' ex ')' | ID | INTLITERAL""" grade 7
  
  this add bnfgrammar"""ex -> subParse E | '(' ex ')'
  subParse -> X F | '(' subParse ')' | '(' subParse ')' F
  E -> '+' subParse E |
  F -> '*' X F | X F | / X F |
  X -> ID | INTLITERAL""" grade 5 comment "3*(X) ?"
  
  //this add bnfgrammar"""""" grade 0
  
  this add bnfgrammar"""E3 -> E4 | E5
  E4 -> E5 '+' E3
  E5 -> E6 | E7 | E8 | E10
  E6 -> E10 / E5
  E7 -> E10 E5
  E8 -> E10 '*' E5
  E10 -> ID | INTLITERAL | '(' E3 ')'""" grade 4 comment "E10 on rule E6, E7 and E8?"*/
    
}