package grammarcomp

package parsing
import grammar._
import CFGrammar._
import utils.Util.MultiMap
import utils.Util.TrieMap
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.LinkedHashSet
import scala.collection.mutable.HashSet
import scala.annotation.tailrec
import java.io._
import scala.collection.immutable.Range
import ParseTreeUtils._
import java.util.Calendar

trait Edge
  object Predict extends Edge {
    override def toString = "Predict"
  }
  object Scan extends Edge {
    override def toString = "Scan"
  }
  object Complete extends Edge 
  {
    override def toString = "Complete"
  }
/** Represents an Earley item, described as a rule, a position of a point in the rule, 
 *  and the starting point of the parsing for the said rule.
 *  It is represented as (A -> B . C , i), the dot being where point is pointing.
 */
case class EarleyItem[T](rule: Rule[T], 
                         point: Int, 
                         start: Int,
                         edge: Edge) {
  var parent: EarleyItem[T] = null
  var predicted_by: HashSet[EarleyItem[T]] = new HashSet();
  var seen = false
  
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
      // Ignore the parent !
      case EarleyItem(r, p, s, e) => (r==rule)&&(p==point)&&(s==start)
      case _ => false
    }
  }
  
  def isComplete(other: Any) = {
    other match {
      // Ignore the parent !
      case EarleyItem(r, p, s, e) => (r==rule)&&((p-1)==point)&&(s==start)
      case _ => false
    }
  }
}

case class TempTree[T](rule: Rule[T],
                         parent: TempTree[T], 
                         term: Option[Terminal[T]] = None){
    var children: List[TempTree[T]] = List()
    
    override def toString() = {
      rule.toString() + "\n   " + children.reverse.map(x=>if (x.term.isDefined) x.term.get else x.rule.toString())
    }
    
    def toPTree(): ParseTree[T] = {
      if (term.isDefined) {
        new PLeaf(term.get)
      }
      else {
        new PNode(rule, children.reverse.map { x => x.toPTree })
      }
    }
  }

class EarleyParser[T](G: Grammar[T]) extends Parser[T] {
  //require(isIn2NF(G, false))
  
  private var enable_cache = false
  private def getRules: List[Rule[T]] = G.rules
  private val axiom = G.start
  
 
  // Store the derivation for future uses?
  private var input: List[Terminal[T]] = List()
  private var parsingTable: Array[HashSet[EarleyItem[T]]] = new Array[HashSet[EarleyItem[T]]](0)
  private var length: Int = 0
  private var table = false
  private var endItem: Option[EarleyItem[T]] = None;
  private var parsingTree: TempTree[T] = null
  private var parseGraph: Option[List[(EarleyItem[T], Edge)]]= None;
  
  def setCache(enable: Boolean = true): Unit = {
    enable_cache = true
  }
  
  /**
   * Flush the cache
   */
  def clear(): Unit = {
    parsingTable = new Array[HashSet[EarleyItem[T]]](0)
    length = 0
    table=false
    endItem = None
    parsingTree = null
    parseGraph = None
  }
  
  override def toString(): String = {
    var str = ""
    parsingTable.foreach{x => {
      x.foreach { y => str += y + ", " }
      str = "[" + str + "]\n"
    }
    }
    "[" + str + "]"
  }
  
  def isParsable(w: List[Terminal[T]]): Boolean = {
    parsingTable(w.length).exists { item => 
          (item.rule.leftSide equals axiom) &&
          (item.start == 0) &&
          {
           if (item.rule.rightSide drop item.point equals List()) {
              endItem = Some(item)
              true
           } 
           else false
          }
    }
  }
  
  def parse(w: List[Terminal[T]])(implicit opctx: GlobalContext): Boolean = {
    opctx.stats.updateCounter(1, "EarleyParseCalls")
    var timer = Calendar.getInstance.getTimeInMillis
    //val timer = new Stats.Timer()

    computeTable(w)
    // If an axiomatic rule has been completed at the end, the string is recognized.
    timer = Calendar.getInstance.getTimeInMillis - timer
    opctx.stats.updateCounterTime(timer, "EarleyParseTime", "EarleyParseCalls")
    isParsable(w)
  }
  
  def parseWithTree(s: List[Terminal[T]])(implicit opctx: GlobalContext): Option[ParseTree[T]] = {
    opctx.stats.updateCounter(1, "EarleyParseTreeCalls")
    var timer = Calendar.getInstance.getTimeInMillis
    if (parsingTree!=null) {
      timer = Calendar.getInstance.getTimeInMillis - timer
      opctx.stats.updateCounterTime(timer, "EarleyParseTime", "EarleyParseTreeCalls")
      return Some(parsingTree.toPTree())
    }
    if (!table) {
      computeTable(s)
    }
    if (!isParsable(s)) {
      return None
    }
    val L = parseGraph.getOrElse(graphParcour())
    //println(L)
    val (item, edge) = L.head
    val tree = 
    if (item.rule.rightSide.length==1 && (edge==Scan)) {
      parsingTree = new TempTree(item.rule, null, Some(item.rule.rightSide.head.asInstanceOf[Terminal[T]]))
      //parsingTree.end = 0
      //parsingTree.start = 0
      Some(new PLeaf(item.rule.rightSide.head.asInstanceOf[Terminal[T]]))
    }
    else {
      val tt = constructTree(L, new TempTree[T](item.rule, null), 0)
      parsingTree = tt
      //println(ParseTreeDSL.mapTree(tt))
      Some(tt.toPTree())
    }
    timer = Calendar.getInstance.getTimeInMillis - timer
    opctx.stats.updateCounterTime(timer, "EarleyParseTime", "EarleyParseTreeCalls")
    tree
  }
  
  def parseWithTrees(s: List[Terminal[T]])(implicit opctx: GlobalContext): InternalFeedback[T] = {
    opctx.stats.updateCounter(1, "EarleyParseTreeCalls")
    var timer = Calendar.getInstance.getTimeInMillis
    if (parsingTree!=null) {
      timer = Calendar.getInstance.getTimeInMillis - timer
      opctx.stats.updateCounterTime(timer, "EarleyParseTime", "EarleyParseTreeCalls")
      return new Parsed(Stream(parsingTree.toPTree()))
    }
    if (!table) {
      computeTable(s)
    }
    if (!isParsable(s)) {
      val index = findLastNotEmpty(0)
      var L: HashSet[Terminal[T]] = HashSet()
      parsingTable(index).foreach { x => {val term = (x.rule.rightSide drop x.point)
                                          term match {
                                            case (t:Terminal[T])::_ => L.add(t)
                                            case _ =>
                                          }
                                          }}
      return EarleyFeedback(index, L.toList)
    }
    val L = parseGraph.getOrElse(graphParcour())
    //println(L)
    val (item, edge) = L.head
    val tree = 
    if (item.rule.rightSide.length==1 && (edge==Scan)) {
      parsingTree = new TempTree(item.rule, null, Some(item.rule.rightSide.head.asInstanceOf[Terminal[T]]))
      //parsingTree.start = 0
      //parsingTree.end = 0
      new PLeaf(item.rule.rightSide.head.asInstanceOf[Terminal[T]])
    }
    else {
      val tt = constructTree(L, new TempTree[T](item.rule, null), 0)
      parsingTree = tt
      //println(ParseTreeDSL.mapTree(tt))
      tt.toPTree
    }
    timer = Calendar.getInstance.getTimeInMillis - timer
    opctx.stats.updateCounterTime(timer, "EarleyParseTreeTime", "EarleyParseTreeCalls")
    new Parsed(Stream(tree))
  }
  
  /**
   * @param: w modified string
   * @param: i starting point of modification in original string
   * @param: j ending point of modification in original string
   * @param: k ending point of the modification in new string
   * 
   * Computes the parsing table once again, with a few accelerations.
   */
  def update(w: List[Terminal[T]], i: Int, j: Int, k: Int)(implicit opctx: GlobalContext): Unit = {
    if (i==w.length) {
      return
    }
    val oldlength = length
    val newLength = w.length + 1
    input = w
    length = newLength
    val oldPT = parsingTable.map(identity)
    
    parsingTable = new Array[HashSet[EarleyItem[T]]](newLength)
    //println(oldPT.length, parsingTable.length)
    val offset = newLength - oldlength//k-j
    //println("offset: "+ offset)
    if (i>0) {
      // Copy the old table up till the changes
      for(n<-Range(0, i+1)) {parsingTable(n) = oldPT(n)}
      
      // Apply earley algorithm for the new stuff
      for(n<-Range(i+1, newLength)) {parsingTable(n) = new HashSet()}
      applyEarley(w, i, k+1)
      
      // If the parsing graph was computed, we can use it to further accelerate.
      if (parseGraph.isDefined) {
        applyEarleyWithCheck(w, oldPT, i, k, newLength-1, offset)
      } else {
        applyEarley(w, k, newLength-1)
      }
      // Flush cache
      parseGraph = None
      parsingTree = null
      table = true
    } else {
      parseGraph = None
      parsingTree = null
      table = false
      endItem = None
      computeTable(w)
    }
  }
  
   /**
   * @param: w modified string
   * 
   * Computes the parsing table once again, with a few accelerations.
   * Computes the difference between old string and w.
   */
  def update(w: List[Terminal[T]])(implicit opctx: GlobalContext): Unit = {
    val (i, j) = getPrefixSuffix(input, w)
    val k = w.length - j
    update(w, i, input.length - j, if (k<i) i else k)
  }

  /** In the parsing table, returns the last entry that is not empty
   *  When used, set index to 0.
   */
  @tailrec private def findLastNotEmpty(index: Int): Int = {
    if (index==length) {
      return index
    }
    if (parsingTable(index).isEmpty) {
      index - 1
    } else {
      findLastNotEmpty(index+1)
    }
  }
  
  /** Applies the graph traversal
   */
  private def graphParcour(): List[(EarleyItem[T], Edge)] = {
    if (endItem.isEmpty) {
      return null
    }
    parseGraph = Some(gpAux(endItem.get, List(), null, List()))
    return parseGraph.get
  }
  
  
  /** Not so simple graph parcour, goes up the parents.
   *  Performs a little trick when seeing a complete edge:
   *  Store what was completed and forces to take it when seeing the next predict.
   */
  @tailrec private def gpAux(ei: EarleyItem[T], 
                             L: List[(EarleyItem[T], Edge)], 
                             edge: Edge,
                             to_complete: List[EarleyItem[T]]): List[(EarleyItem[T], Edge)] = {
    
    if (ei!=null) {ei.seen=true}
    if ((ei==null)&&(!to_complete.isEmpty)) {
      var final_L = L
      to_complete.foreach { item => final_L = 
              (new EarleyItem(item.rule, item.point-1, item.start, Predict), Predict)::final_L }
      return final_L
    } 
    else if (ei==null) {
      L
    }
    else {
      edge match {
        case Complete =>
          gpAux(ei.parent, (ei, edge)::L, ei.edge, L.head._1::to_complete)
        case Predict =>
          val (prec, _) = L.head
          if (to_complete.isEmpty) {
                println(ei, edge)
                println(ei.parent, ei.edge)
                println(L)
          }
          val item = to_complete.head
          val valid = 
            if (prec.predicted_by.size==1) {
              Some(prec.predicted_by.head)
            } else {
              prec.predicted_by.find { x => x.isComplete(item)}
            }
          gpAux(valid.get.parent, (valid.get, edge)::L, valid.get.edge, to_complete.tail)
        case _ => gpAux(ei.parent, (ei, edge)::L, ei.edge, to_complete)
      }
    }
  }
  
  /**
   * Given a well-formed List of earley items and edges sorted by order of production,
   * returns a Temporary Tree corresponding to this list.
   * tt is the supporting tree, corresponding to the earley item at the head of the list
   */
  @tailrec private final def constructTree(L: List[(EarleyItem[T], Edge)],
                                   tt: TempTree[T],
                                   index: Int): TempTree[T] = {
    if (L.isEmpty) { // Shouldn't be encountered
      //tt.end = index
      return tt
    }
    val (item, edge) = L.head
    val tail = L.tail
    if (edge==null) {
      //tt.end=index
      return tt
    }
    edge match {
      // Scan produces directly a child.
      case Scan => 
        val ttscan =(new TempTree(item.rule,
                     tt,
                     Some(item.rule.rightSide.drop(item.point).head.asInstanceOf[Terminal[T]])))
        //ttscan.start = index
        //ttscan.end = index
        tt.children = 
          ttscan::tt.children
        constructTree(tail, tt, index+1)
      // Predict should produce a new child, and recursively fill its own children.
      case Predict =>
        val (i1, e1) = tail.head
        val ttpredict = (new TempTree(i1.rule, tt))
        //ttpredict.start = i1.start
        tt.children = ttpredict::tt.children
        constructTree(tail, ttpredict, index)
      // Complete should go back to filling its parent.
      case Complete =>
        //tt.end = index
        constructTree(tail, tt.parent, index)
    }
  }
  
  private def computeTable(w: List[Terminal[T]]): Int = {
    // Initialize the parsing table
    input = w
    length = w.length + 1
    parsingTable = new Array[HashSet[EarleyItem[T]]](length)
    
    for (i <- Range(0, length)) {
      parsingTable(i) = HashSet()
    }
    
    // Set up T[0]
    for (rule <- getRules) {
      if (rule.leftSide == axiom) {
        parsingTable(0).add(new EarleyItem(rule, 0, 0, null)) 
      }
    }
    for (item <- parsingTable(0)) {
      zeroPredictor(item)
    }
    
    applyEarley(w, 0, length-1)
    
    // Finish the job
    for (item <- parsingTable(length-1)) {
      discriminator(None, item, length-1)
    }
    table = true;
    return length-1
    //println(this)
  }
  
  private def applyEarley(w: List[Terminal[T]], i: Int, j:  Int): Int = {
    for ((term, index) <- ((w drop i take (j-i)) zip Range(i, j))) {
      if (parsingTable(index).isEmpty) {
        return index
      }
      for(item <- parsingTable(index)) {
        discriminator(Some(term), item, index)
      }
    }
    return j-1
  }
  
  private def applyEarleyWithCheck(w: List[Terminal[T]], 
                           oldPT: Array[HashSet[EarleyItem[T]]],
                           i: Int,
                           start: Int,
                           j: Int,
                           offset: Int): Int = {
    for ((term, index) <- ((w drop start take (j-start)) zip Range(start, j))) {
      if (parsingTable(index).isEmpty) {
        return index
      }
      for(item <- parsingTable(index)) {
        discriminator(Some(term), item, index)(true)
      }
      for(item <- parsingTable(index)) {
        if (item.start<=i) {
          /* if in the old table, there is an item s.t.:
           * - it started before the modifications
           * - it is in a place corresponding to what we just added, and it is equal,
           * - it is useful for constructing the tree.
           * Then, we can stop applying earley, and simply use the derivations from the old
           * input.
           */
          val optei = oldPT(index-offset).find(x=>(x.seen)&&((x equals item)))
          if (optei.isDefined) {
            //println("took the update at: " + index)
            //println(optei.get)
            val L = parseGraph.get
            var item_index = index
            val tail = L.dropWhile({case (ei, e)=> !(ei equals item)}).tail
            // at least 1 element
            if (!tail.isEmpty) {
              val (ei, edge) = tail.head
              if (ei.edge==Scan) {item_index+=1}
              if (item.point==item.rule.rightSide.length) {
                // Make sure to set a new parent for the corresponding item.
                val newEI = new EarleyItem(ei.rule, ei.point, ei.start, Complete)
                ei.parent = item
                ei.predicted_by = item.predicted_by
              }
              else {
                val newEI = new EarleyItem(ei.rule, ei.point, ei.start, Predict)
                ei.parent = item
                ei.predicted_by = HashSet(item)
              }
            }
            var par = item
            tail.foreach({case (ei, edge) => 
              ei.parent = par
              ei.edge match {
                // Black magic happening?
                case Predict =>
                  ei.predicted_by = HashSet(par)
                case _ => ei.predicted_by = par.predicted_by
              }
              try {parsingTable(item_index).add(ei)} catch {case e: Exception => println(ei)}
              if (edge==Scan) {item_index+=1}
              par = ei
            })
            return j-1
          }
        }
      }
    }
    for (item <- parsingTable(j-1)) {
      discriminator(None, item, j-1)
    }
    return j-1
  }
    
    
  // A step for setting up T[0]
  private def zeroPredictor(item: EarleyItem[T]): Unit = {
    val restOfRule = item.rule.rightSide drop item.point
    restOfRule match {
      case (nt@Nonterminal(sym))::_ => 
        getRules.foreach { grammarRule =>
        if (grammarRule.leftSide == (nt)) {
          val earleyItem = new EarleyItem(grammarRule, 0, 0, Predict)
          earleyItem.parent = item
            if (parsingTable(0).add(earleyItem)) { 
              zeroPredictor(earleyItem)
            } /*else {
              parsingTable(0).update(earleyItem, true)
            }*/
        }
      }
      case _ =>
    }
  }
  
  private def discriminator(term: Option[Terminal[T]], item: EarleyItem[T], index: Int)
  (implicit check: Boolean = false): Unit = {
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
  
  private def scanner(item: EarleyItem[T], index: Int)
  (implicit check: Boolean): Unit = {
    if (index<length) {
      val earleyItem = new EarleyItem(item.rule, item.point+1, item.start, Scan)
      earleyItem.parent = item
      parsingTable(index+1).add(earleyItem)
    }
  }
  
  private def predictor(nt: Nonterminal, item: EarleyItem[T], index: Int, term: Option[Terminal[T]])
  (implicit check: Boolean): Unit = {
    getRules.foreach { grammarRule =>  
        if (grammarRule.leftSide == (nt)) {
          val earleyItem = new EarleyItem(grammarRule, 0, index, Predict)
          val test = parsingTable(index).find(x => x equals earleyItem)
          earleyItem.parent = item
          /*if (parsingTable(index).add(earleyItem)) {
            discriminator(term, earleyItem, index)
          }*/
          if (test.isDefined) {
            test.get.predicted_by.add(item)
          } else {
            earleyItem.predicted_by = HashSet(item)
            parsingTable(index).add(earleyItem)
            discriminator(term, earleyItem, index)
          }
        }
     }
   }
  
  private def completor(item: EarleyItem[T], index: Int, term: Option[Terminal[T]])
  (implicit check: Boolean): Unit = {
    val A = item.rule.leftSide
    for(to_advance <- parsingTable(item.start)) {
      to_advance.rule.rightSide drop to_advance.point match {
        case (nt@Nonterminal(sym))::_  if nt == A => 
          val earleyItem = new EarleyItem(to_advance.rule, 
                                          to_advance.point + 1, 
                                          to_advance.start,
                                          Complete)
          earleyItem.parent = item
          earleyItem.predicted_by = item.predicted_by
          if(parsingTable(index).add(earleyItem)) {
            discriminator(term, earleyItem, index)
          } /*else {
            parsingTable(index).update(earleyItem, true)
          }*/
        case _ =>
      }
    }
  }
  
  @tailrec private final def prefixIndex(L1: List[Terminal[T]], L2: List[Terminal[T]], i: Int = 0): Int = {
    (L1, L2) match {
      case (Nil, _) => i
      case (_, Nil) => i
      case (a::q, h::t) if a.obj == h.obj => prefixIndex(q, t, i+1)
      case _ => i
    }
  }
  
  private def suffixIndexReverse(L1: List[Terminal[T]], L2: List[Terminal[T]]): Int = {
    prefixIndex(L1.reverse, L2.reverse)
  }
  
  private def getPrefixSuffix(L1: List[Terminal[T]], L2: List[Terminal[T]]): (Int, Int) = {
    val i = prefixIndex(L1, L2)
    val j = suffixIndexReverse(L1, L2)
    (i, j)
  }
}