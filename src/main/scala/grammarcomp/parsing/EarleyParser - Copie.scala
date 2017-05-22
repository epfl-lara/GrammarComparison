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

class EarleyParsernot[T](G: Grammar[T]) extends Parser[T] {
  //require(isIn2NF(G, false))
  
  def getRules: List[Rule[T]] = G.rules
  val axiom = G.start
  
  /** Represents an Earley item, described as a rule, a position of a point in the rule, 
   *  and the starting point of the parsing for the said rule.
   *  It is represented as (A -> B . C , i), the dot being where point is pointing.
   */
  case class EarleyItem[T](rule: Rule[T], 
                           point: Int, 
                           start: Int,
                           edge: Edge) {
    var parent: HashSet[EarleyItem[T]] = new HashSet();
    var predicted_by: HashSet[EarleyItem[T]] = new HashSet();
    var seen = false;
    
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
 
  // Store the derivation for future uses?
  var parsingTable: Array[HashSet[EarleyItem[T]]] = new Array[HashSet[EarleyItem[T]]](0)
  var length: Int = 0
  var endItem: Option[EarleyItem[T]] = None;
  
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
    if (!parse(s)) {
      return None
    }
    val L = graphParcour()
    println(L)
    val (item, edge) = L.head
    if (item.rule.rightSide.length==1 && (edge==Scan)) {
      Some(new PLeaf(item.rule.rightSide.head.asInstanceOf[Terminal[T]]))
    }
    else {
      val tt = constructTree(L, new TempTree[T](item.rule, null)).toPTree
      println(ParseTreeDSL.mapTree(tt))
      Some(tt)
    }
  }
  def parseWithTrees(s: List[Terminal[T]])(implicit opctx: GlobalContext): InternalFeedback[T] = {
    new Parsed(graphParcour2().map(x => constructTree(x, new TempTree[T](x.head._1.rule, null)).toPTree))
  }

  
  
  def graphParcour(): List[(EarleyItem[T], Edge)] = {
    if (endItem.isEmpty) {
      return null
    }
    gpAux(endItem.get, List(), null, List())
  }
  
  def graphParcour2(): Stream[List[(EarleyItem[T], Edge)]] = {
     Stream.cons(gpAux2(endItem.get, List(), null, List()), graphParcour2())
  }
  
  @tailrec private def gpAux2(ei: EarleyItem[T], 
                              L: List[(EarleyItem[T], Edge)], 
                              edge: Edge,
                              to_complete: List[EarleyItem[T]]): List[(EarleyItem[T], Edge)] = {
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
      ei.seen = true
      edge match {
        case Complete =>
          val unseen = ei.parent.find { x => !x.seen }
          if (unseen.isDefined) {
            gpAux2(unseen.get, (ei, edge)::L, ei.edge, L.head._1::to_complete)
          } else {
            val (item, e) = L.head
            val tail = L.tail
            val (prec, _) = tail.head
            gpAux2(item, tail, e, if (prec equals to_complete.head) to_complete.tail else to_complete)
          }
        case Predict =>
          val item = to_complete.head
          val (prec, _) = L.head
          val valid = prec.predicted_by.find{ x => x.isComplete(item) && (x.parent.exists( y => !y.seen))}
          if (valid.isDefined) {
            val valpar = valid.get.parent.find( x => !x.seen).get //we know it exists
            gpAux2(valpar, (valid.get, edge)::L, valid.get.edge, to_complete.tail)
          } else {
            val (item, e) = L.head
            val tail = L.tail
            val (prec, _) = tail.head
            gpAux2(item, tail, e, if (prec equals to_complete.head) to_complete.tail else to_complete)
          }
           //gpAux2(valid.get.parent, (valid.get, edge)::L, valid.get.edge, to_complete.tail)
        //case _ => gpAux2(ei.parent, (ei, edge)::L, ei.edge, to_complete)
        case Scan =>
          val unseen = ei.parent.find { x => !x.seen }
          if (unseen.isDefined) {
            gpAux2(unseen.get, (ei, edge)::L, ei.edge, to_complete)
          } else {
            val (item, e) = L.head
            val tail = L.tail
            val (prec, _) = tail.head
            gpAux2(item, tail, e, if (prec equals to_complete.head) to_complete.tail else to_complete)
          }
        case _ => 
          val unseen = ei.parent.find { x => !x.seen }
          if (unseen.isDefined) {
            gpAux2(unseen.get, (ei, edge)::L, ei.edge, to_complete)
          } else {
            List()
          }
      }
    }
  }
  
  
  /** Not so simple graph parcour, takes the first item that produced another.
   *  store the information about what is the "last" item to be completed.
   *  That way, when we see a predict, we know which parent to take !
   */
  @tailrec private def gpAux(ei: EarleyItem[T], 
                             L: List[(EarleyItem[T], Edge)], 
                             edge: Edge,
                             to_complete: List[EarleyItem[T]]): List[(EarleyItem[T], Edge)] = {
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
          gpAux(ei.parent.head, (ei, edge)::L, ei.edge, L.head._1::to_complete)
        case Predict =>
          val item = to_complete.head
          val (prec, _) = L.head
          val valid = prec.predicted_by.find { x => x.isComplete(item)}
          gpAux(valid.get.parent.head, (valid.get, edge)::L, valid.get.edge, to_complete.tail)
        case _ => gpAux(ei.parent.head, (ei, edge)::L, ei.edge, to_complete)
      }
    }
  }
  
  /**
   * Given a well-formed List of earley items and edges sorted by order of production,
   * returns a Temporary Tree corresponding to this list.
   * tt is the supporting tree, corresponding to the earley item at the head of the list
   */
  @tailrec final def constructTree(L: List[(EarleyItem[T], Edge)],
                                   tt: TempTree[T]): TempTree[T] = {
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
        tt.children = 
          (new TempTree(item.rule,
                        tt,
                        Some(item.rule.rightSide.drop(item.point).head.asInstanceOf[Terminal[T]])))::tt.children
        constructTree(tail, tt)
      // Predict should produce a new child, and recursively fill its own children.
      case Predict =>
        val (i1, e1) = tail.head
        val ttpredict = (new TempTree(i1.rule, tt))
        tt.children = ttpredict::tt.children
        constructTree(tail, ttpredict)
      // Complete should go back to filling its parent.
      case Complete =>
        constructTree(tail, tt.parent)
    }
  }
  
  def computeTable(w: List[Terminal[T]]): Int = {
    // Initialize the parsing table
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
    
    // Apply the algorithm
    for ((term, index) <- (w zip Range(0, length-1))) { // will go 'til length - 2, tho...
      if (parsingTable(index).isEmpty) {
        return index
      }
      for(item <- parsingTable(index)) {
        discriminator(Some(term), item, index)
      }
    }
    
    // Finish the job
    for (item <- parsingTable(length-1)) {
      discriminator(None, item, length-1)
    }
    return length-1
    //println(this)
  }
    
  // A step for setting up T[0]
  def zeroPredictor(item: EarleyItem[T]): Unit = {
    val restOfRule = item.rule.rightSide drop item.point
    restOfRule match {
      case (nt@Nonterminal(sym))::_ => 
        getRules.foreach { grammarRule =>
        if (grammarRule.leftSide == (nt)) {
          val earleyItem = new EarleyItem(grammarRule, 0, 0, Predict)
          val here = parsingTable(0).find(x=>x equals earleyItem)
          if (here.isDefined) {
            here.get.parent.add(item)
            here.get.predicted_by.add(item)
          } else {
            earleyItem.parent = HashSet(item)
            earleyItem.predicted_by = HashSet(item)
            parsingTable(0).add(earleyItem)
          }
          /*earleyItem.parent = item
            if (parsingTable(0).add(earleyItem)) { 
              zeroPredictor(earleyItem)*/
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
    println("scanner sucks")
    if (index<length) {
      val earleyItem = new EarleyItem(item.rule, item.point+1, item.start, Scan)
      earleyItem.parent.add(item)
      earleyItem.predicted_by = item.predicted_by
      parsingTable(index+1).add(earleyItem)
    }
  }
  
  def predictor(nt: Nonterminal, item: EarleyItem[T], index: Int, term: Option[Terminal[T]]): Unit = {
    println("predictor sucks")
    getRules.foreach { grammarRule =>  
        if (grammarRule.leftSide == (nt)) {
          val earleyItem = new EarleyItem(grammarRule, 0, index, Predict)
          val test = parsingTable(index).find(x => x equals earleyItem)
          earleyItem.parent = HashSet(item)
          /*if (parsingTable(index).add(earleyItem)) {
            discriminator(term, earleyItem, index)
          }*/
          if (test.isDefined) {
            test.get.predicted_by.add(item)
            test.get.parent.add(item)
          } else {
            earleyItem.predicted_by = HashSet(item)
            parsingTable(index).add(earleyItem)
            discriminator(term, earleyItem, index)
          }
        }
     }
   }
  
  def completor(item: EarleyItem[T], index: Int, term: Option[Terminal[T]]): Unit = {
    println("completor sucks")
    val A = item.rule.leftSide
    for(to_advance <- parsingTable(item.start)) {
      to_advance.rule.rightSide drop to_advance.point match {
        case (nt@Nonterminal(sym))::_  if nt == A => 
          val earleyItem = new EarleyItem(to_advance.rule, 
                                          to_advance.point + 1, 
                                          to_advance.start,
                                          Complete)
          earleyItem.parent = HashSet(item)
          earleyItem.predicted_by = item.predicted_by
          val test = parsingTable(index).find(x => x equals earleyItem)
          if (test.isDefined) {
            test.get.parent.add(item)
            test.get.predicted_by.union(item.predicted_by)
          }
          else {
            parsingTable(index).add(earleyItem)
            discriminator(term, earleyItem, index)
          }
          /*if(parsingTable(index).add(earleyItem)) {
            discriminator(term, earleyItem, index)
          }*/
        case _ =>
      }
    }
  }
}