package grammarcomp

/**
 * File:    Generator.scala
 * Date:    20/5/2013
 * Author:  Mikaël Mayer
 * Purpose: Generates up to a certain depth the possibles words that the grammar can produce.
 */
package generators

import grammar._
import CFGrammar._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.{ Set => MutableSet }
import CFGrammar._
import utils.Util

/**
 * Generator for this grammar.
 */
object GeneratorBottomup {
  /**
   * grammar - A grammar in CNF form
   * number - number of strings to be generated
   * The 'separator' argument is only used for printing
   */
  def apply[T](grammar: Grammar[T], maxWords: Int, debug : Boolean = false): List[List[Terminal[T]]] = {
    val g = new GenerationScheme(grammar, maxWords, debug)
    val r = g.run
    r
  }

  class GenerationScheme[T](grammar: Grammar[T], maxWords: Int, debug : Boolean) {

    private var yields = Map[Nonterminal, Words[T]]()

    def rewriteRules(s: Symbol[T]) = grammar.rules.filter(_.leftSide == s)

    //truncated to maxWords
    def cartesianProduct(words1: Words[T], words2: Words[T]) = {
      var i = 0
      var product = List[(Word[T], Word[T])]()
      for (w1 <- words1; w2 <- words2; if i <= maxWords) {
        i += 1
        product :+= (w1, w2)
      }
      product
    }

    def ruleApplies(rule: Rule[T]): Boolean = {
      rule.rightSide.forall {
        case nt: Nonterminal => yields.contains(nt) && !yields(nt).isEmpty
        case _ => true
      }
    }

    def applyRule(rule: Rule[T]): Words[T] = {
      val Rule(lhs, rhs) = rule
      if (rhs.isEmpty) List(List()) //generate empty word
      else {
        val listOfWords = rhs.map(r => r match {
          case t: Terminal[T] => List(List(t))
          case nt: Nonterminal => yields(nt).toList
        })
        if (listOfWords.isEmpty) {
          //nothing can be generated
          List()
        } else {
          //compute the cartesian product of all the words
          val head :: tail = listOfWords
          val words = tail.foldLeft(head)((acc, words) => {
            val product = cartesianProduct(acc, words)
            product.map { case (w1, w2) => w1 ++ w2 }.distinct
          })
          words
        }
      }
    }

    /**
     * Returns the newly generated words for the nonterminal
     */
    def gen(nt: Nonterminal): Words[T] = {
      val rules = rewriteRules(nt)
      val newwords = rules.filter(ruleApplies).flatMap(applyRule).distinct

      var oldWords = List[Word[T]]()
      if (yields contains nt) {
        oldWords = yields(nt)
        yields -= nt
      }
      yields += (nt -> (oldWords ++ newwords).distinct)
      newwords
    }

    /**
     * Sorts nonterminals in topological order.
     * TODO: make this more efficient using graphs and dfs algorithm
     */
    def nonterminalsInTopologicalOrder[T](grammar: Grammar[T]): List[Nonterminal] = {

      def insert(index: Int, list: List[Nonterminal], s: Nonterminal) = {
        var i = 0
        var head = List[Nonterminal]()
        list.foreach((elem) => {
          if (i == index)
            head :+= s
          head :+= elem
          i += 1
        })
        head
      }

      /*def calls(left: Nonterminal, right: Nonterminal): Boolean = {        
        grammar.rules.exists {          
          case Rule[T](l, r) if l == left && r.contains(right) =>
            if(debug)
              println(left + "--calls-->"+ right) 
            true
          
          case _ => false
        }
      }*/
      
      //construct a transitive call set for each  nonterminal
      val reachSet = Util.fixpoint((reachSet : Map[Nonterminal,Set[Nonterminal]]) => {
        val newpairs = grammar.rules.flatMap(rule => {
          val newreach = rule.rightSide.flatMap { 
            case nt: Nonterminal => reachSet.getOrElse(nt, Set()).toList :+ nt
            case _ => List()
          }          
          newreach.map(nt => rule.leftSide -> nt) 
        }).groupBy(_._1)
        var newMap = newpairs.map{
          case (k,v) => k -> (reachSet.getOrElse(k, Set()) ++ v.map(_._2))          
        }.toMap
        newMap ++= (reachSet.keySet -- newMap.keySet).map(k => (k -> reachSet(k)))  
        newMap        
      })(Map[Nonterminal,Set[Nonterminal]]())
      
      def calls(left: Nonterminal, right: Nonterminal): Boolean = 
        if(reachSet.contains(left)) 
          reachSet(left).contains(right)
        else 
          false

      var topologicalOrder = List[Nonterminal]()
      grammar.rules.map(_.leftSide).foreach(nt => {
        if (!topologicalOrder.contains(nt)) {
          var foundPosition = false
          var index = 0
          for (i <- 0 to topologicalOrder.length - 1) {
            if (!foundPosition && calls(topologicalOrder(i), nt)) {
              index = i
              foundPosition = true
            }
          }
          if (!foundPosition)
            topologicalOrder :+= nt
          else
            topologicalOrder = insert(index, topologicalOrder, nt)
        }
      })
      topologicalOrder
    }

    def run: Words[T] = {

      val nonterms = nonterminalsInTopologicalOrder(grammar)
      if(debug)
        println("After topological sort: "+nonterms)
      //assert(nonterms.last == grammar.start)

      val f = (_: Unit) => {
        nonterms.foreach(gen)
        if(debug)
          yields.foreach(entry => println(entry._1 +" --> "+wordsToString(entry._2)))                
      }
      var oldSize = 0
      val pred = (_: Unit) => {
        if (yields.contains(grammar.start)) {
          val size = yields(grammar.start).size
          if (size >= maxWords || size == oldSize)
            true
          else {
            oldSize = size
            false
          }
        } else true
      }
      Util.repeatUntil(f, pred)((): Unit)
      if (yields.contains(grammar.start))
        yields(grammar.start).toList
      else
        List()
    }
  }
}