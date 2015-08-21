package grammar.examples
import grammar._
import CFGrammar._
import GrammarReaders.GrammarImplicit

object Quiz2010 extends QuizResult {
  import GrammarReaders._
  import QuizResult._
  
  implicit def MAX_GRADE = 2
  
  override def quizName = "Quiz2010"
    
  override def reference = bnfgrammar"""S -> Int V
  V -> $$ | => Int V | , Int W  
  W -> , Int W | => Int V"""
  
  //another reference solution
  this add bnfgrammar"""S0 -> S $$
  S -> T => S | Int
  T -> S , T | Int""" 

  this add bnfgrammar"""S0 -> S $$
  S -> T F
  F -> => S
  T -> S G
  G -> , S
  S -> Int
  T -> Int""" not correct comment "We asked for LL(1), maybe you were looking for LL(1)"
    
  this add bnfgrammar"""S0 -> S $$
  S -> T => Int | Int
  T -> Int | S , Int""" not correct
  
  this add bnfgrammar"""S -> => S | $$ S | Int S | , S | "" """ not correct comment "Sigma star - strategy 1"
  
  this add bnfgrammar"""S -> (=>  | $$ | Int | ,)*""" is correct comment "Sigma star"     
     
  this add bnfgrammar"""S0 -> S $$
  S -> Int T
  T -> , S | => S | "" """ is correct        
    
  this add bnfgrammar"""S0 -> S $$
  S -> T => Int
  T -> T => T0
  T0 -> Int C
  C -> , T0 | "" """ not correct comment "The grammar does not generate Int => Int. Missing rule T -> Int maybe?"    
  
  this add bnfgrammar"""S0 -> S $$
  S -> T => S
  S -> T , S
  S -> T
  T -> Int""" not correct comment "The LL(1) entry for S contains two rules"

  this add bnfgrammar"""S0 -> S $$
  S -> Ta S
  Ta -> T A
  A -> =>
  T -> Sc T
  Sc -> S C
  C -> ,
  T -> Int
  S -> Int""" not correct comment "We wanted LL(1) not CYK. Not LL(1) since duplication in its LL(1) table"
  
  this add bnfgrammar"""S0 -> Q $$
  Q -> Q => Q | Q , Q | Int"""
  
  this add bnfgrammar"""S0 -> S $$
  S -> Int Sp
  Sp -> ""  | T0 => S
  T0 -> Sp , T |  ""
  T -> Int T0""" not correct comment "Does not make Int=>Int, mistakenly makes =>Int"
  
  this add bnfgrammar"""S0 -> S $$
  S -> T => S | Term
  T -> S , T | Term
  Term -> Int""" not correct comment "This is the same as the original grammar"

  this add bnfgrammar"""S0 -> S $$
S -> T => S | Int
T -> Int , T | Int
""" not correct comment "We asked for LL(1) not unambiguous"
}