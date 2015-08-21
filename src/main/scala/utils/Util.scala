package grammar
package utils

import scala.annotation.tailrec
import scala.collection.mutable.{Map => MutableMap}

/**
 * A collection of helper methods.
 */
object Util {

  /**
   * Applies f repeatedly starting from 'v' until a fixpoint is reached.
   */
  def fixpoint[A](f: A => A)(v: A): A = {
    val r = f(v)
    if (r != v) fixpoint(f)(r)
    else r
  }

  /**
   * Apply the function on its result until a predicate holds
   */
  def repeatUntil[A](f: A => A, pred: A => Boolean)(v: A): A = {
    val r = f(v)
    if (!pred(r)) repeatUntil(f, pred)(r)
    else r
  }

  /**
   * Creates a fresh name using a given prefix if any
   */
  var uniqueId: Long = 1
  def freshName(prefix: Option[String] = None): String = {
    uniqueId += 1
    val suffix = "-" + uniqueId
    prefix match {
      case Some(name) => name + suffix
      case _ => "N" + suffix
    }
  }
  
  def freshNumber : Long =  {
    uniqueId += 1
    uniqueId
  }

  def setString[T](seq: Seq[T]) = {
    "{" + seq.mkString(",") + "}"
  }
  
  def removeAtIndex[T](l: List[T], index: Int) : List[T]= {
    l.take(index) ++ l.drop(index + 1)    
  }
  
  def matrixToString[T](mat : Array[Array[T]]) : String = {
    mat.map(_.mkString(" ")).mkString("\n")
  }
  
  def pipeUntilFailure[A](checks : List[A => Boolean], arg: A) = {
    checks.foldLeft(true){
      case (true, check) => check(arg)
      case _ => false 
    }
  }

  /**
   * Bounded cartesian product. The bound is optional
   */
  def cartesianProduct[T](words1: List[T], words2: List[T], maxNOW: Option[Int] = None) = {
    var i = 0
    var product = List[(T, T)]()
    for (w1 <- words1; w2 <- words2; if (!maxNOW.isDefined || i <= maxNOW.get)) {
      i += 1
      product :+= (w1, w2)
    }
    product
  }

  /**
   * Integer square root of bigints.
   * Uses Newton-Raphson method
   */
  val one = BigInt(1)
  def sqrt(number: BigInt) = {
    def next(n: BigInt, i: BigInt): BigInt = (n + i / n) >> 1    
    var n = one
    var n1 = next(n, number)
    while ((n1 - n).abs > one) {
      n = n1
      n1 = next(n, number)
    }
    while (n1 * n1 > number) {
      n1 -= one
    }
    n1
  }

  /**
   * Ceil of the square root of a bigint
   * Uses Newton-Raphson method
   */
  def ceilSqrt(number: BigInt) = {
    def next(n: BigInt, i: BigInt): BigInt = (n + i / n) >> 1
    var n = one
    var n1 = next(n, number)
    while ((n1 - n).abs > one) {
      n = n1
      n1 = next(n, number)
    }
    var n1sqr = n1 * n1
    while (n1sqr > number) {
      n1 -= one
      n1sqr = n1 * n1
    }
    if (n1sqr < number)
      n1 + 1
    else
      n1
  }

  /**
   * Schedules execution of the task after a specified timeout
   */
  def scheduleTask(callBack: () => Unit, timeOut: Long): Option[java.util.Timer] = {
    if (timeOut > 0) {
      val timer = new java.util.Timer();
      timer.schedule(new java.util.TimerTask() {
        def run() {
          callBack()          
          timer.cancel() //the timer will be cancelled after it runs
        }
      }, timeOut);
      Some(timer)
    } else None
  }

  def registerShutdownHook(task: () => Unit) = {
    Runtime.getRuntime().addShutdownHook(new Thread() {
      override def run() {
        task()
      }
    })
  }
  
  object MultiSet {
    def apply[A]() = {
      new MultiSet[A](List[A]())
    }
    def apply[A](list : List[A])  = {
      new MultiSet[A](list)
    }
  }
  
  class MultiSet[A](val contents: List[A]) {
    //lazy val baseSet = contents.distinct  
    lazy val counts = {
      val mmap = MutableMap[A,Int]()
      contents.foreach{
        case a if mmap.contains(a) => 
          mmap(a) += 1
        case a => 
          mmap += (a -> 1)
      }
      mmap.toMap
    }
    
    def prepend(a: A) : MultiSet[A] = {
      new MultiSet[A](a +: contents)
    }
    
    def +(a : A) : MultiSet[A] = {
      new MultiSet[A](contents :+ a)
    }
    
    override def hashCode = {
      counts.hashCode
    }
    
    override def equals(other : Any) = other match {
      case ms : MultiSet[A] if counts == ms.counts =>
        true
      case _ =>
        false 
    }
    
    def multiplicity(a: A) = {
      contents.count(_ == a)
    }
    
    def isEmpty : Boolean = {
      contents.isEmpty
    }        
    
    override def toString  = {
      Util.setString(contents)
    } 
  }

  class MultiMap[A, B] extends scala.collection.mutable.HashMap[A, scala.collection.mutable.Set[B]] with scala.collection.mutable.MultiMap[A, B] {

    /**
     * Creates a new map and does not change the existing map
     */
    def append(that: MultiMap[A, B]): MultiMap[A, B] = {
      val newmap = new MultiMap[A, B]()
      this.foreach { case (k, vset) => newmap += (k -> vset) }
      that.foreach {
        case (k, vset) => vset.foreach(v => newmap.addBinding(k, v))
      }
      newmap
    }
  }

  /**
   * Allows duplicate entries
   */
  class OrderedMultiMap[A, B] extends scala.collection.mutable.HashMap[A, scala.collection.mutable.ListBuffer[B]] {

    def addBinding(key: A, value: B): this.type = {
      get(key) match {
        case None =>
          val list = new scala.collection.mutable.ListBuffer[B]()
          list += value
          this(key) = list
        case Some(list) =>
          list += value
      }
      this
    }

    /**
     * Creates a new map and does not change the existing map
     */
    def append(that: OrderedMultiMap[A, B]): OrderedMultiMap[A, B] = {
      val newmap = new OrderedMultiMap[A, B]()
      this.foreach { case (k, vlist) => newmap += (k -> vlist) }
      that.foreach {
        case (k, vlist) => vlist.foreach(v => newmap.addBinding(k, v))
      }
      newmap
    }

    /**
     * Make the value of every key distinct
     */
    def distinct: OrderedMultiMap[A, B] = {
      val newmap = new OrderedMultiMap[A, B]()
      this.foreach { case (k, vlist) => newmap += (k -> vlist.distinct) }
      newmap
    }

  }

  /**
   * Implements a mapping from Seq[A] to B
   */
  final class TrieMap[A, B] {
    var childrenMap = Map[A, TrieMap[A, B]]()
    var dataMap = Map[A, B]()

    @tailrec def addBinding(key: Seq[A], value: B) {
      key match {
        case Seq() =>
          throw new IllegalStateException("Key is empty!!")
        case Seq(x) =>
          //add the value to the dataMap
          if (dataMap.contains(x))
            throw new IllegalStateException("A mapping for key already exists: " + x + " --> " + dataMap(x))
          else
            dataMap += (x -> value)
        case head +: tail => //here, tail has at least one element      
          //check if we have an entry for seq(0) if yes go to the children, if not create one
          val child = childrenMap.getOrElse(head, {
            val ch = new TrieMap[A, B]()
            childrenMap += (head -> ch)
            ch
          })
          child.addBinding(tail, value)
      }
    }

    @tailrec def lookup(key: Seq[A]): Option[B] = {
      key match {
        case Seq() =>
          throw new IllegalStateException("Key is empty!!")
        case Seq(x) =>
          dataMap.get(x)
        case head +: tail => //here, tail has at least one element      
          childrenMap.get(head) match {
            case Some(child) =>
              child.lookup(tail)
            case _ => None
          }
      }
    }
  }

}