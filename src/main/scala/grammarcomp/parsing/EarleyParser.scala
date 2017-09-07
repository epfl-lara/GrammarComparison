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
  var parent: HashEarley[T] = new HashEarley;
  //var predicted_by: HashEarley[T] = new HashEarley();
  var seen = false
  var seen_by: HashEarley[T] = new HashEarley
  
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
    (ruleHash + point * 1061 + start * 17)
  }
  override def equals(other: Any) = {
    other match {
      // Ignore the parent !
      case EarleyItem(r, p, s, e) => (r equals rule)&&(p==point)&&(s==start)
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

class HashEarley[U] {
  var size = 8
  var buckets: Array[List[EarleyItem[U]]] = 
    Array(Nil, Nil, Nil, Nil, Nil, Nil, Nil, Nil)
  
  private var contained = 0
  
  def this(elem: EarleyItem[U]) = {
    this()
    this.add(elem)
  }
  
  override def toString = {
    var str = "HashEarley("
    buckets.foreach { x => {x.foreach { y => str+=y.toString() + ", "}}}
    str + ")"
  }
  
  implicit class IntWithMod(x: Int) {
    def mod(y: Int) = {
      val modulo = x%y
      modulo + (if (modulo < 0) y else 0)
    }
  }
  
  def maintain() = {
    if (2*contained>3*size) {
      size = size * 2
      val old = buckets
      //var newBucks: Array[List[EarleyItem[U]]] = new Array(size)
      buckets = (new Array[List[EarleyItem[U]]](size)).map { x => Nil }
      old.foreach { x => x.foreach { y => simpleAdd(y) } }
    }
  }
  
  def add(elem: EarleyItem[U]) = {
    val bucket = buckets(elem.hashCode() mod size)
    val (list, b) = addWithoutDouble(bucket, elem)
    if (b) {
      buckets(elem.hashCode() mod size) = list
      contained += 1
      maintain()
    }
    b
  }
  
  def remove(elem: EarleyItem[U]) = {
    val bucket = buckets(elem.hashCode() mod size)
    buckets(elem.hashCode() mod size) = bucket.filterNot { x => x equals elem }
  }
  
  def get(elem: EarleyItem[U]) = {
    val bucket = buckets(elem.hashCode() mod size)
    bucket.find { x => x equals elem }
  }
  
  def getAny(): Option[EarleyItem[U]] = {
    buckets.foreach { x => x.foreach { y => return Some(y) } }
    return None
  }
  
  def getUnseen(curr: EarleyItem[U]): Option[EarleyItem[U]] = {
    for (bucket <- buckets;
         x <- bucket) {
      if ((!x.seen)||(x.seen_by.get(curr).isEmpty)) {return Some(x)}
    }
    return None
  }
  
  def getSeen(elem: EarleyItem[U]) = {
    val bucket = buckets(elem.hashCode() mod size)
    bucket.find { x => (x equals elem)&&(x.seen) }
  }
  
  def getUnseenAndComplete(to_complete: EarleyItem[U], curr: EarleyItem[U]): Option[EarleyItem[U]] = {
    val elem = new EarleyItem(to_complete.rule, to_complete.point-1, to_complete.start, null)
    val bucket = buckets(elem.hashCode() mod size)
    val res = bucket.find { x => (x equals elem)&&(x.seen_by.get(curr).isEmpty)}
    if (res.isDefined) {
    println
    println(res)
    println
    println(res.get.seen_by)
    println
    println(curr)}
    res
  }
  
  /*def getUnseenAndComplete(to_complete: EarleyItem[U], curr: EarleyItem[U]): Option[EarleyItem[U]] = {
    for (bucket <- buckets;
         x <- bucket) {
      if ((x.isComplete(to_complete)&&({
        println("seen_by: " + x.seen_by);
        println(to_complete)
        println(curr)
        println
        x.seen_by.get(curr).isEmpty
      }))) {return Some(x)}
    }
    return None
  }*/
  
  def getComplete(to_complete: EarleyItem[U]): Option[EarleyItem[U]] = {
    val elem = new EarleyItem(to_complete.rule, to_complete.point-1, to_complete.start, null)
    val bucket = buckets(elem.hashCode() mod size)
    bucket.find { x => (x equals elem)}
  }
  
  /*def getComplete(to_complete: EarleyItem[U]): Option[EarleyItem[U]] = {
    for (bucket <- buckets;
         x <- bucket) {
      if ((x.isComplete(to_complete))) {return Some(x)}
    }
    return None
  }*/
  
  def foreach[A](f: EarleyItem[U] => A) = buckets.foreach { x => x.foreach(f) }
  
  def exists[A](f: EarleyItem[U] => Boolean) = buckets.exists { x => x.exists(f) }
  
  def toList() = buckets.foldLeft[List[EarleyItem[U]]](List())((x: List[EarleyItem[U]], y:List[EarleyItem[U]]) => x.:::(y))
  
  def isEmpty = (contained == 0)
  
  private def addWithoutDouble(L: List[EarleyItem[U]], elem: EarleyItem[U]) = {
    @tailrec def aux(List: List[EarleyItem[U]], acc: List[EarleyItem[U]], elem: EarleyItem[U]): (List[EarleyItem[U]], Boolean) = {
      List match {
        case Nil => (elem::acc, true)
        case t::q if t equals elem => (L, false)
        case t::q => aux(q, t::acc, elem)
      }
    }
    aux(L, Nil, elem)
  }
  
  private def simpleAdd(elem: EarleyItem[U]) = {
    val bucket = buckets(elem.hashCode() mod size)
    buckets(elem.hashCode() mod size) = elem::bucket
  }
}

/**
 * How to use:
 * - Create an instance of EarleyParser with the grammar.
 * - On calling parse, the parsing table is computed and cached, but no tree is.
 * - On calling parseWithTree(s), if the parsing table in not in cache, compute it, then compute the tree and cache it.
 * 
 * If you want to parse another input without incremental, you have to clear the cache beforehand by calling clear()
 * When calling update only with the list of terminals, it will compute automatically the difference between the last input and the new list.
 * If the difference is known beforehand, call update with the following parameters:
 * - the list of terminals,
 * - the index where the difference begins,
 * - the index where the difference ends in the original string,
 * - the index where the difference ends in the new string.
 */
class EarleyParser[T](G: Grammar[T]) extends Parser[T] {
  //require(isIn2NF(G, false))
  
  private def getRules: List[Rule[T]] = G.rules
  private val axiom = G.start
  
 
  // Store the derivation for future uses?
  private var input: List[Terminal[T]] = List()
  private var parsingTable: Array[HashEarley[T]] = new Array[HashEarley[T]](0)
  private var length: Int = 0
  private var table = false
  private var endItem: Option[EarleyItem[T]] = None;
  private var parsingTree: TempTree[T] = null
  private var parseGraph: Option[List[(EarleyItem[T], Edge)]]= None;
  private var predictedGraph: List[EarleyItem[T]] = null;
  
  /**
   * Flush the cache
   */
  def clear(): Unit = {
    parsingTable = new Array[HashEarley[T]](0)
    length = 0
    table=false
    endItem = None
    parsingTree = null
    parseGraph = None
  }
  
  /**
   * a simple way to print it. Looks horrible.
   */
  override def toString(): String = {
    var str = ""
    parsingTable.foreach{x => {
      x.foreach { y => str += y + ", " }
      str = "[" + str + "]\n"
    }
    }
    "[" + str + "]"
  }
  
  private def isParsable(w: List[Terminal[T]]): Boolean = {
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
  
  /**
   * return true if w is parsable, returns false otherwise.
   */
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
  
  /**
   * returns some parse tree if s is parsable, returns None otherwise
   */
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
    val (item, edge) = L.head
    val tree = 
    if (item.rule.rightSide.length==1 && (edge==Scan)) {
      parsingTree = new TempTree(item.rule, null, Some(item.rule.rightSide.head.asInstanceOf[Terminal[T]]))
      //parsingTree.end = 0
      //parsingTree.start = 0
      Some(new PLeaf(item.rule.rightSide.head.asInstanceOf[Terminal[T]]))
    }
    else {
      val tt = constructTree(L, new TempTree[T](item.rule, null), 0, s)
      parsingTree = tt
      Some(tt.toPTree())
    }
    timer = Calendar.getInstance.getTimeInMillis - timer
    opctx.stats.updateCounterTime(timer, "EarleyParseTime", "EarleyParseTreeCalls")
    tree
  }
  
  /**
   * Returns a feedback on the parsing of s
   */
  def parseWithTrees(s: List[Terminal[T]])(implicit opctx: GlobalContext): InternalFeedback[T] = {
    opctx.stats.updateCounter(1, "EarleyParseTreeCalls")
    var timer = Calendar.getInstance.getTimeInMillis
    /*if (parsingTree!=null) {
      timer = Calendar.getInstance.getTimeInMillis - timer
      opctx.stats.updateCounterTime(timer, "EarleyParseTime", "EarleyParseTreeCalls")
      return new Parsed(Stream(parsingTree.toPTree()))
    }*/
    if (!table) {
      computeTable(s)
    }
    if (!isParsable(s)) {
      val index = findLastNotEmpty(0)
      var L: HashSet[Terminal[T]] = new HashSet()
      parsingTable(index).foreach { x => {val term = (x.rule.rightSide drop x.point)
                                          term match {
                                            case (t:Terminal[T])::_ => L.add(t)
                                            case _ =>
                                          }
                                          }}
      return EarleyFeedback(index, L.toList)
    }
    val L = parseGraph.getOrElse(graphParcour())
    val (item, edge) = L.head
    val tree = 
    if (item.rule.rightSide.length==1 && (edge==Scan)) {
      parsingTree = new TempTree(item.rule, null, Some(item.rule.rightSide.head.asInstanceOf[Terminal[T]]))
      new PLeaf(item.rule.rightSide.head.asInstanceOf[Terminal[T]])
    }
    else {
      val tt = constructTree(L, new TempTree[T](item.rule, null), 0, s)
      parsingTree = tt
      tt.toPTree
    }
    timer = Calendar.getInstance.getTimeInMillis - timer
    opctx.stats.updateCounterTime(timer, "EarleyParseTreeTime", "EarleyParseTreeCalls")
    new Parsed(Stream.cons(tree, Stream.continually({
      val L = multipleGraph()
      val tree = 
      if (item.rule.rightSide.length==1 && (edge==Scan)) {
        parsingTree = new TempTree(item.rule, null, Some(item.rule.rightSide.head.asInstanceOf[Terminal[T]]))
        new PLeaf(item.rule.rightSide.head.asInstanceOf[Terminal[T]])
      }
      else {
        val tt = constructTree(L, new TempTree[T](item.rule, null), 0, s)
        parsingTree = tt
        tt.toPTree
      }
      tree
    })))
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
    try {
      if (i==w.length) {
        return
      }
      val oldlength = length
      val newLength = w.length + 1
      input = w
      length = newLength
      // Copy and unsee everything...?
      unsee()
      val oldPT = parsingTable.map(identity)
      
      parsingTable = new Array[HashEarley[T]](newLength)
      val offset = newLength - oldlength//k-j
      if (i>0) {
        // Copy the old table up till the changes
        for(n<-Range(0, i+1)) {parsingTable(n) = oldPT(n)}
        
        // Apply earley algorithm for the new stuff
        for(n<-Range(i+1, newLength)) {parsingTable(n) = new HashEarley()}
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
    } catch {
    case e: java.lang.ArrayIndexOutOfBoundsException => 
        println("something unexpected happened, execute regular parsing")
        clear()
        computeTable(w)
    case e: java.util.NoSuchElementException =>
        println("something unexpected happened, execute regular parsing")
        clear()
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
  
  /** Applies a single graph traversal
   */
  private def graphParcour(): List[(EarleyItem[T], Edge)] = {
    if (endItem.isEmpty) {
      return null
    }
    //parseGraph = Some(gpAux(endItem.get, List(), null, List()))
    parseGraph = Some(gpTry(endItem.get, null, List(), List(), List()))
    return parseGraph.get
  }
  
  private def multipleGraph(): List[(EarleyItem[T], Edge)] = {
    println(parseGraph.get)
    parseGraph = Some(backtrack(parseGraph.get.head._1, parseGraph.get, List(), predictedGraph))
    println(parseGraph.get)
    return parseGraph.get
  }
  
  private def backtrack(prec: EarleyItem[T], 
                        L: List[(EarleyItem[T], Edge)], 
                        to_complete: List[EarleyItem[T]], 
                        predicted: List[EarleyItem[T]]): List[(EarleyItem[T], Edge)] = {
    println("________________________")
    println
    println(prec + ", " + prec.edge.toString())
    println 
    println("parents: "+prec.parent)
    println
    println("seen_by: "+ prec.seen_by)
    println
    println("L: " + L)
    println
    println("predicted: " + predicted)
    println
    println("to_complete: " + to_complete)
    val (grandParent, e) = L.tail.head
    println
    println("grandparent: " + grandParent)
    grandParent.edge match {
      case Scan =>
        gpTry(prec, grandParent, L.tail, to_complete, predicted, true)
      case Complete =>
        gpTry(prec, grandParent, L.tail, to_complete.tail, predicted, true)
      case Predict =>
        gpTry(prec, grandParent, L.tail, predicted.head::to_complete, predicted.tail, true)
    }
  }
  
  @tailrec private def gpTry(curr: EarleyItem[T],
                             prec: EarleyItem[T],
                             L: List[(EarleyItem[T], Edge)],
                             to_complete: List[EarleyItem[T]],
                             predicted: List[EarleyItem[T]],
                             want_unseen: Boolean = false): List[(EarleyItem[T], Edge)] = {
    if (curr equals prec) {
      println("what the fuck?")
      println(curr)
      println(curr.seen_by)
      println(to_complete)
    }
    if (prec!=null) {
      curr.seen_by.add(prec)
    }
    if (curr.parent.isEmpty&&(!want_unseen)) {
      curr.seen = true
      if (prec!=null) {curr.seen_by.add(prec)}
      val outEdge = (if (prec==null) null else prec.edge)
      predictedGraph = to_complete:::predicted
      (curr, outEdge)::L
    } else {
      curr.edge match { // Ingoing edge
        case Complete => 
          // TODO: Seen
          val par =
          if (want_unseen) {
            curr.parent.getUnseen(curr)
          } else {
            curr.parent.getAny
          }
          par match {
            case None =>
              if (prec==null) {List()}
              else {
                backtrack(prec, L, to_complete, predicted)
              }
            case Some(item) =>
              curr.seen = true
              if (prec!=null) {curr.seen_by.add(prec)}
              val outEdge = (if (prec==null) null else prec.edge)
              gpTry(item, curr, (curr, outEdge)::L, curr::to_complete, predicted)
          }
          
        case Predict =>
          // TODO: Seen
          if (to_complete.isEmpty) {
            curr.seen = true
            if (prec!=null) {curr.seen_by.add(prec)}
            val outEdge = (if (prec==null) null else prec.edge)
            predictedGraph = predicted
            (curr, outEdge)::L
          }
          else {
            val to_predict = to_complete.head
            val predicted_parent =
              if(want_unseen) {
                println(curr, prec)
                curr.parent.getUnseenAndComplete(to_predict, curr)
              } else {
                curr.parent.getComplete(to_predict)
              }
            predicted_parent match {
              case None =>
                if (prec==null) {List()}
                else {
                  backtrack(prec, L, to_complete, predicted)
                }
              case Some(item) =>
                curr.seen = true
                if (prec!=null) {curr.seen_by.add(prec)}
                val outEdge = (if (prec==null) null else prec.edge)
                gpTry(item, curr, (curr, outEdge)::L, to_complete.tail, to_predict::predicted)
            }
          }
          
        case Scan => 
          // TODO: Seen
          val par =
          if (want_unseen) {
            curr.parent.getUnseen(curr)
          } else {
            curr.parent.getAny
          }
          par match {
            case None =>
              if (prec==null) {List()}
              else {
                backtrack(prec, L, to_complete, predicted)
              }
            case Some(item) =>
              curr.seen = true
              if (prec!=null) {curr.seen_by.add(prec)}
              val outEdge = (if (prec==null) null else prec.edge)
              gpTry(item, curr, (curr, outEdge)::L, to_complete, predicted)
          }
        case _ => //Should not happen
          println("Match Error in gpTry")
          List()
      }
    }
  }
  
  /** Not so simple graph parcour, goes up the parents.
   *  Performs a little trick when seeing a complete edge:
   *  Store what was completed and forces to take it when seeing the next predict.
   */
  /**@tailrec private def gpAux(ei: EarleyItem[T], 
                             L: List[(EarleyItem[T], Edge)], 
                             edge: Edge,
                             to_complete: List[EarleyItem[T]]): List[(EarleyItem[T], Edge)] = {
    
    //if (ei!=null) {ei.seen=true}
    if ((ei==null)&&(!to_complete.isEmpty)) {
      var final_L = L
      var par: EarleyItem[T] = null
      to_complete.foreach { item => final_L = {
                val newEi = new EarleyItem(item.rule, item.point-1, item.start, Predict)
                newEi.parent = HashEarley(par)
                par = newEi
                (newEi, Predict)::final_L 
              }
            }
      return final_L
    } 
    else if (ei==null) {
      L
    }
    else {
      edge match {
        case Complete =>
          val par = ei.parent.find { x => !x.seen }
          par match {
            case None => 
              if (L.isEmpty) {
                return List()
              }
              val outEdge = L.head._1.edge
              gpAux(L.head._1, L.tail, outEdge, if (outEdge==Complete) to_complete.tail else to_complete)
            case Some(item) => 
              ei.seen = true
              gpAux(item, (ei, edge)::L, ei.edge, L.head._1::to_complete)
          }
          //gpAux(ei.parent, (ei, edge)::L, ei.edge, L.head._1::to_complete)
        case Predict =>
          val (prec, _) = L.head
          /*if (to_complete.isEmpty) {
            println(ei, edge)
            println(ei.parent, ei.edge)
            println(L)
          }*/
          val item = to_complete.head
          var valid = // Should ALWAYS be found
            if (prec.parent.size==1) {
              Some(prec.parent.head)
            } else {
              prec.parent.find { x => x.isComplete(item)}
            }
          if (valid.isEmpty) {
            //println(to_complete)
          }
          gpAux(valid.get.parent.head, (valid.get, edge)::L, valid.get.edge, to_complete.tail)
        case _ => gpAux(ei.parent.head, (ei, edge)::L, ei.edge, to_complete)
      }
    }
  }**/
  
  /**
   * Given a well-formed List of earley items and edges sorted by order of production,
   * returns a Temporary Tree corresponding to this list.
   * tt is the supporting tree, corresponding to the earley item at the head of the list
   */
  @tailrec private final def constructTree(L: List[(EarleyItem[T], Edge)],
                                   tt: TempTree[T],
                                   index: Int,
                                   w: List[Terminal[T]]): TempTree[T] = {
    if (L.isEmpty) { // Shouldn't be encountered
      return tt
    }
    val (item, edge) = L.head
    val tail = L.tail
    if (edge==null) {
      return tt
    }
    edge match {
      // Scan produces directly a child.
      case Scan => 
        val ttscan =(new TempTree(item.rule,
                     tt,
                     Some(w.head)))
                     //Some(item.rule.rightSide.drop(item.point).head.asInstanceOf[Terminal[T]])))
        tt.children = 
          ttscan::tt.children
        constructTree(tail, tt, index+1, w.tail)
      // Predict should produce a new child, and recursively fill its own children.
      case Predict =>
        val (i1, e1) = tail.head
        val ttpredict = (new TempTree(i1.rule, tt))
        //ttpredict.start = i1.start
        tt.children = ttpredict::tt.children
        constructTree(tail, ttpredict, index, w)
      // Complete should go back to filling its parent.
      case Complete =>
        constructTree(tail, tt.parent, index, w)
    }
  }
  
  private def computeTable(w: List[Terminal[T]]): Int = {
    // Initialize the parsing table
    input = w
    length = w.length + 1
    parsingTable = new Array[HashEarley[T]](length)
    
    for (i <- Range(0, length)) {
      parsingTable(i) = new HashEarley()
    }
    
    // Set up T[0]
    for (rule <- getRules) {
      if (rule.leftSide == axiom) {
        //parsingTable(0).add(new EarleyItem(rule, 0, 0, null)) 
        parsingTable(0).add(new EarleyItem(rule, 0, 0, Predict)) 
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
                           oldPT: Array[HashEarley[T]],
                           i: Int,
                           start: Int,
                           j: Int,
                           offset: Int): Int = {
    for ((term, index) <- ((w drop start take (j-start)) zip Range(start, j))) {
      if (parsingTable(index).isEmpty) {
        return index
      }
      for(item <- parsingTable(index)) {
        discriminator(Some(term), item, index)
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
          val optei = oldPT(index-offset).getSeen(item)
          if (optei.isDefined) {
            println("took the update at: " + index)
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
                ei.parent = new HashEarley(item)
                //ei.predicted_by = item.predicted_by
              }
              else {
                val newEI = new EarleyItem(ei.rule, ei.point, ei.start, Predict)
                ei.parent = new HashEarley(item)
                //ei.predicted_by = HashEarley(item)
              }
            }
            var par = item
            tail.foreach({case (ei, edge) => 
              ei.seen = false
              ei.parent = new HashEarley(par)
              /**ei.edge match {
                // Black magic happening?
                case Predict =>
                  ei.predicted_by = HashEarley(par)
                case _ => 
                  ei.predicted_by = par.predicted_by
              }**/
              //parsingTable(item_index).add(ei)
              updateSet(parsingTable(item_index), ei)
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
  
  private def updateSet[U](set: HashEarley[U], elem: EarleyItem[U]): Unit = {
    if (!set.add(elem)) {
      set remove elem
      set add elem
    }
  }
    
  // A step for setting up T[0]
  private def zeroPredictor(item: EarleyItem[T]): Unit = {
    val restOfRule = item.rule.rightSide drop item.point
    restOfRule match {
      case (nt@Nonterminal(sym))::_ => 
        getRules.foreach { grammarRule =>
        if (grammarRule.leftSide == (nt)) {
          val earleyItem = new EarleyItem(grammarRule, 0, 0, Predict)
          earleyItem.parent = new HashEarley(item)
            /*if (parsingTable(0).add(earleyItem)) { 
              zeroPredictor(earleyItem)
            }*/
          val test = parsingTable(0).get(earleyItem)
          if (test.isDefined) {
            test.get.parent.add(item)
          } else {
            //earleyItem.predicted_by = HashEarley(item)
            parsingTable(0).add(earleyItem)
            zeroPredictor(earleyItem)
          }
        }
      }
      case _ =>
    }
  }
  
  private def discriminator(term: Option[Terminal[T]], item: EarleyItem[T], index: Int): Unit = {
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
  
  private def scanner(item: EarleyItem[T], index: Int): Unit = {
    if (index<length) {
      val earleyItem = new EarleyItem(item.rule, item.point+1, item.start, Scan)
      earleyItem.parent = new HashEarley(item)
      parsingTable(index+1).add(earleyItem)
    }
  }
  
  private def predictor(nt: Nonterminal, item: EarleyItem[T], index: Int, term: Option[Terminal[T]]): Unit = {
    getRules.foreach { grammarRule =>  
        if (grammarRule.leftSide == (nt)) {
          val earleyItem = new EarleyItem(grammarRule, 0, index, Predict)
          val test = parsingTable(index).get(earleyItem)
          earleyItem.parent = new HashEarley(item)
          /*if (parsingTable(index).add(earleyItem)) {
            discriminator(term, earleyItem, index)
          }*/
          if (test.isDefined) {
            test.get.parent.add(item)
          } else {
            //earleyItem.predicted_by = HashEarley(item)
            parsingTable(index).add(earleyItem)
            discriminator(term, earleyItem, index)
          }
        }
     }
   }
  
  private def completor(item: EarleyItem[T], index: Int, term: Option[Terminal[T]]): Unit = {
    val A = item.rule.leftSide
    for(to_advance <- parsingTable(item.start)) {
      to_advance.rule.rightSide drop to_advance.point match {
        case (nt@Nonterminal(sym))::_  if nt == A => 
          val earleyItem = new EarleyItem(to_advance.rule, 
                                          to_advance.point + 1, 
                                          to_advance.start,
                                          Complete)
          earleyItem.parent = new HashEarley(item)
          val test = parsingTable(index).get(earleyItem)
          if (test.isDefined) {
            test.get.parent.add(item)
          } else {
            //earleyItem.predicted_by = HashEarley(item)
            parsingTable(index).add(earleyItem)
            discriminator(term, earleyItem, index)
          }
          //earleyItem.predicted_by = item.predicted_by
          /*if(parsingTable(index).add(earleyItem)) {
            discriminator(term, earleyItem, index)
          } */
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
  
  private def unsee(): Unit = {
    parsingTable.foreach { x => x.foreach { y => y.seen=false } }
  }
  
  def getParseGraph = parseGraph.getOrElse(graphParcour)
}