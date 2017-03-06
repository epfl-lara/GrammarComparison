package grammarcomp

package parsing
import grammar._
import CFGrammar._
//import utils.Util.MultiMap
//import utils.Util.TrieMap
//import scala.collection.mutable.ListBuffer
//import scala.collection.mutable.LinkedHashSet
//import scala.collection.mutable.HashSet
//import java.io._
//import scala.collection.immutable.Range
//import ParseTreeUtils._

class EarleyParser[T](G: Grammar[T]) extends Parser[T] {
  require(isIn2NF(G, false))
  
  def getRules: List[Rule[T]] = G.rules
  val axiom = G.start
  
  /** Represents an Earley item, described as a rule, a position of a point in the rule, 
   *  and the starting point of the parsing for the said rule.
   *  It is represented as (A -> B . C , i), the dot being where point is pointing.
   */
  case class EarleyItem[T](rule: Rule[T], point: Int, start: Int) {
    override def toString() = {
      var str = ""
      for((term, i) <- rule.rightSide zip Range(0, rule.rightSide.length)) {
        if (i == point) {str += ". " + term + " "}
        else {str += term + " "}
      }
      if (point == rule.rightSide.length) {
        str += "."
      }
      "(" + rule.leftSide + " -> " + str + ", " + start + ")"
    }
    override def hashCode() = {
      val ruleHash = rule.hashCode()
      (ruleHash + point * 31 + start * 41)
    }
    override def equals(other: Any) = {
      other match {
        case EarleyItem(r, p, s) => (r==rule)&&(p==point)&&(s==start)
        case _ => false
      }
    }
  }
 
  def testEquals(): Boolean = {
    val ei1 = new EarleyItem(getRules.head, 0, 0)
    val ei2 = new EarleyItem(getRules.head, 0, 0)
    return ei1 equals ei2
  }
  
  // Store the derivation for future uses?
  var parsingTable: Array[List[EarleyItem[T]]] = new Array[List[EarleyItem[T]]](0)
  var length: Int = 0
  override def toString(): String = {
    var str = ""
    parsingTable.foreach{x => {
      x.foreach { y => str += y + ", " }
      str = "[" + str + "]\n"
    }
    }
    "[" + str + "]"
  }
  
  def parse(w: List[Terminal[T]])(implicit opctx: GlobalContext): Boolean = {
    //opctx.stats.updateCounter(1, "CYKParseCalls")
    //val timer = new Stats.Timer()

    computeTable(w)
    // If an axiomatic rule has been completed at the end, the string is recognized.
    for(item <- parsingTable(w.length)) {
      if ((item.rule.leftSide equals axiom) &&
          (item.start == 0) &&
          (item.rule.rightSide drop item.point equals List())) {
        return true
      }
    }
    return false
    //opctx.stats.updateCounterTime(timer.timeWithoutGC(), "CYKParseTime", "CYKParseCalls")
  }
  def parseWithTree(s: List[Terminal[T]])(implicit opctx: GlobalContext): Option[ParseTree[T]] = ???
  def parseWithTrees(s: List[Terminal[T]])(implicit opctx: GlobalContext): InternalFeedback[T] = ???

  
  def computeTable(w: List[Terminal[T]]): Unit = {
    // Initialize the parsing table
    length = w.length + 1
    parsingTable = new Array[List[EarleyItem[T]]](length)
    
    for (i <- Range(0, length)) {
      parsingTable(i) = List()
    }
    
    // Set up T[0]
    for (rule <- getRules) {
      if (rule.leftSide == axiom) {
        parsingTable(0) = 
          new EarleyItem(rule, 0, 0) :: parsingTable(0)
      }
    }
    for (item <- parsingTable(0)) {
      zeroPredictor(item)
    }
    
    // Apply the algorithm
    for ((term, index) <- (w zip Range(0, length-1))) { // will go 'til length - 2, tho...
      for(item <- parsingTable(index)) {
        discriminator(Some(term), item, index)
      }
    }
    
    // Finish the job
    for (item <- parsingTable(length-1)) {
      discriminator(None, item, length-1)
    }
    //println(this)
  }
    
  // A step for setting up T[0]
  def zeroPredictor(item: EarleyItem[T]): Unit = {
    val restOfRule = item.rule.rightSide drop item.point
    restOfRule match {
      case (nt@Nonterminal(sym))::_ => 
        getRules.foreach { grammarRule =>
        if (grammarRule.leftSide == (nt)) {
          val earleyItem = new EarleyItem(grammarRule, 0, 0)
          val b = parsingTable(0) exists {_ equals earleyItem}
          if (!b) {
            parsingTable(0) = earleyItem::parsingTable(0)
            zeroPredictor(earleyItem)
          }
        }
      }
      case _ =>
    }
  }
  
  def discriminator(term: Option[Terminal[T]], item: EarleyItem[T], index: Int): Unit = {
    val restOfRule = item.rule.rightSide drop item.point
    restOfRule match {
      /* If an item is of the form (A -> _ . w _, i), where w is the read terminal,
       * add (A -> _ w . _, i) in T[index +1], 
       * ie: advance the lecture.
       */
      case (t: Terminal[T])::_ if (term.isDefined)&&(t equals term.get) => 
        scanner(item, index)
        
      /* If an item is of the form (A -> _ . C _, i) is in T(index),
       * Where C is a non-terminal,
       * For each rule of the form (C -> _) as its left side, 
       * add (C -> . _, index) in T[index],
       * ie: try to recognize C.
       */
      case (nt@Nonterminal(sym))::_ =>
        predictor(nt, item, index, term)
        
      /* If there is an item of the for (A -> _ ., i) in T(index),
       * for each item in T[i] of the form (C -> _ . A _, j),
       * add (C -> _ A . _, j) in T[index],
       * ie: A was recognized.
       */
      case List() => 
        completor(item, index, term)
        
      case _ => 
    }
  }
  
  def scanner(item: EarleyItem[T], index: Int): Unit = {
    if (index<length) {
      val earleyItem = new EarleyItem(item.rule, item.point+1, item.start)
      if (!(parsingTable(index+1).exists { x => earleyItem equals x })) {
        parsingTable(index+1) = earleyItem::parsingTable(index+1)
      }
    }
  }
  
  def predictor(nt: Nonterminal, item: EarleyItem[T], index: Int, term: Option[Terminal[T]]): Unit = {
      getRules.foreach { grammarRule =>  
        if (grammarRule.leftSide == (nt)) {
          val earleyItem = new EarleyItem(grammarRule, 0, index)
          if (!(parsingTable(index).exists { x => earleyItem equals x })) {
            parsingTable(index) = earleyItem::parsingTable(index)
            discriminator(term, earleyItem, index)
          }
        }
     }
   }
  
  def completor(item: EarleyItem[T], index: Int, term: Option[Terminal[T]]): Unit = {
    val A = item.rule.leftSide
    for(to_advance <- parsingTable(item.start)) {
      to_advance.rule.rightSide drop to_advance.point match {
        case (nt@Nonterminal(sym))::_  if nt == A => 
          val earleyItem = new EarleyItem(to_advance.rule, to_advance.point + 1, to_advance.start)
          if (!(parsingTable(index).exists { x => earleyItem equals x })) {
            parsingTable(index) = earleyItem :: parsingTable(index)
            discriminator(term, earleyItem, index)
          }
        case _ =>
      }
    }
  }
}