package grammar

import CFGrammar._
import EBNFGrammar._
import scala.language.implicitConversions

/**
 * A DSL for creating ENBNF grammars and accessing parse trees
 */
object EBNFGrammarDSL {  
  // a symbol can be converted to a NontermId
  implicit def symbolToRegId(sym: scala.Symbol) = new NontermId(sym)
  // anything can be converted to a GenericRegId, symbols to NontermId, and empty strings to RegEmpty
  implicit def anyToRegExp[T](obj: T) =
    obj match {
      case sym: scala.Symbol        => new NontermId(sym)
      case s: String if s.isEmpty() => new RegEmpty()
      case _                        => new Term(obj)
    } 
}

object GrammarDSL {
  // a symbol can be converted to a Nonterminal
  implicit def symbolToNonterm(sym: scala.Symbol) = Nonterminal(sym)
  // anything can be converted to a GenericRegId, symbols to NontermId, and empty strings to RegEmpty
  implicit def anyToTerminal[T](obj: T) =
    obj match {
      case sym: scala.Symbol        => Nonterminal(sym)
      case _                        => Terminal(obj)
    }    
  def epsilon[T]() = Symbols(List[Symbol[T]]())  
}