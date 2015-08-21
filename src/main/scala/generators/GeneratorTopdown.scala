/**
 * File:    Generator.scala
 * Date:    20/5/2013
 * Author:  Mikaël Mayer
 * Purpose: Generates up to a certain depth the possibles words that the grammar can produce.
 */
package generators

import grammar._
import scala.collection.mutable.ArrayBuffer
import CFGrammar._

/**
 * Generator for this grammar.
 */
object GeneratorTopdown {
  def apply(grammar: Grammar, depth: Int, separator: String = ""): List[List[Terminal]] = {
    val g = new GenerationScheme(grammar: Grammar)
    val r = g.run(depth)
    r
  }

  class GenerationScheme(grammar: Grammar) {
    private val generated = ArrayBuffer[List[List[Symbol]]]()

    def isTerminal(s: Symbol) =
      s match {
        case s: Terminal => true
        case _ => false
      }

    def startSymbol = grammar.start

    def rewriteRules(s: Symbol) = grammar.rules.filter(_.leftSide == s)

    def run(depth: Int): List[List[Terminal]] = {

      for (i <- generated.size until depth) { // We incrementally augment the set
        val newSet: List[List[Symbol]] = if (i == 0) List(List(startSymbol))
        else
          for (
            prev <- generated(i - 1); // For each previously generated sequence of symbols.         
            j <- 0 until prev.size; // Replace one of its non-terminal by the rule righthandside
            s = prev(j);
            rules = rewriteRules(s);
            rule <- rules
          ) yield prev.take(j) ++ rule.rightSide ++ prev.drop(j + 1)
        generated += newSet.distinct
      }
      generated.view.take(depth).flatMap(_.filter(_.forall(isTerminal(_))).asInstanceOf[List[List[Terminal]]]).toList
    }
  }

}