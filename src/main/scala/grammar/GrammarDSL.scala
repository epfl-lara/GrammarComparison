package grammar

import CFGrammar._
import EBNFGrammar._
import scala.language.implicitConversions

/**
 * A DSL for creating languages and accessing parse trees
 */
object GrammarDSL {  
  // a symbol can be converted to a NontermId
  implicit def symbolToRegId(sym: scala.Symbol) = new NontermId(sym.toString)
  // anything can be converted to a GenericRegId, symbols to NontermId, and empty strings to RegEmpty
  implicit def anyToRegExp[T](obj: T) =
    obj match {
      case sym: scala.Symbol        => new NontermId(sym.toString)
      case s: String if s.isEmpty() => new RegEmpty()
      case _                        => new Term(obj)
    } 
}