package grammarcomp

package generators

import grammar._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.{ Map => MutableMap, Set => MutableSet }
import CFGrammar._
import grammar.utils._
import scala.annotation.tailrec
import scala.math.BigInt

object RandomAccessGenerator {    
  //a three valued result of querying an element at an index 
  sealed abstract class LookupResult[T]
  case class NoElementAtIndex[T]() extends LookupResult[T]
  case class Element[T](word: List[Terminal[T]]) extends LookupResult[T]
  val bigzero = BigInt(0)
}

/**
 * This generator computes rules and bounds dynamically.
 * 'wordSize' is the maximum size of the word that has to be generated
 */
class SizeBasedRandomAccessGenerator[T](inG: Grammar[T], wordSize: Int)
	(implicit gctx: GlobalContext, opctx: EnumerationContext) {
  import RandomAccessGenerator._
  import RandomAccessGeneratorUtil._

  //remove unproductive rules and reduce arity
  import CNFConverter._
  private val grammar = (simplify[T] _ andThen reduceArity)(inG)

  def reduceArity(inG: Grammar[T]): Grammar[T] = {
    Grammar[T](inG.start, inG.rules.flatMap(reduceArityOfRule))
  }

  def reduceArityOfRule(rule: Rule[T]): List[Rule[T]] = {
    val ntsize = nontermsInRightSide(rule).size
    if (ntsize > 2) {
      //divide the right hand side into half 
      val mid = ntsize / 2
      var i = 0
      var firstHalf = List[Symbol[T]]()
      var secondHalf = List[Symbol[T]]()
      rule.rightSide.foreach {
        case nt: Nonterminal if i < mid =>
          i += 1
          firstHalf :+= nt
        case t: Terminal[T] if i < mid =>
          firstHalf :+= t
        case sym @ _ =>
          secondHalf :+= sym
      }
      //second half will always at least two non-terminals
      val rnt = CFGrammar.freshNonterminal()
      val rightRules = reduceArityOfRule(Rule[T](rnt, secondHalf))
      if (i == 1) {
        //in this case first half has only one non-terminal
        val newRule = Rule[T](rule.leftSide, firstHalf :+ rnt)
        newRule +: rightRules
      } else {
        val lnt = CFGrammar.freshNonterminal()
        val leftRules = reduceArityOfRule(Rule[T](lnt, firstHalf))
        val newRule = Rule[T](rule.leftSide, List(lnt, rnt))
        newRule +: (leftRules ++ rightRules)
      }
    } else
      List(rule)
  }

  //create a word counter for the given size.
  //The word can be queries  for bounds on non-terminals, rules for a given size.
  val wordCounter = new WordCounter(grammar, wordSize)

  //caches from (nonterms/rules to size to index to word)
  type SizeToIndexToWord = MutableMap[Int, MutableMap[BigInt, Word[T]]]
  private var yields = grammar.nonTerminals.map(_ -> MutableMap[Int, MutableMap[BigInt, Word[T]]]()).toMap
  private var ruleYields = grammar.rules.map(_ -> MutableMap[Int, MutableMap[BigInt, Word[T]]]()).toMap

  def lookup[U](keyMap: Map[U, SizeToIndexToWord], key: U, size: Int, index: BigInt): Option[Word[T]] = {
    //only cache indices smaller than a predefined size
    if (index.bitLength <= opctx.maxIndexSizeToCache) {
      val keyYields = keyMap.get(key)
      if (keyYields.isDefined) {
        val sizeYields = keyYields.get.get(size)
        if (sizeYields.isDefined) {
          sizeYields.get.get(index)
        } else
          None
      } else None
    } else None
  }

  def update[U](keyMap: Map[U, SizeToIndexToWord], key: U, size: Int, index: BigInt, w: Word[T]) = {
    //only cache indices smaller than a predefined size    
    if (index.bitLength <= opctx.maxIndexSizeToCache) {
      val keyYields = keyMap.get(key)
      if (keyYields.isDefined) {
        //only cache the non-terminals and rules that are a part of the grammar
        val sizeYields = keyYields.get.get(size)
        if (sizeYields.isDefined) { // && sizeYields.get.size <= opctx.maxCacheSize) {
          sizeYields.get += (index -> w)
        } else
          keyYields.get += (size -> MutableMap[BigInt, Word[T]]())
      }
    }
  }

  def getWordAtIndex(rule: Rule[T], size: Int, index: BigInt): LookupResult[T] = {

    if (opctx.debugGenerator > 1)
      println("Rule[T]: " + rule + " Index: " + index)

    val cachedWord = lookup(ruleYields, rule, size, index)
    if (cachedWord.isDefined) {
      //for stats
      /*opctx.stats.updateCounter(1, "HitCalls")
      opctx.stats.updateCounterStats(index.bitLength, "HitIndexSize", "HitCalls")*/

      Element(cachedWord.get)
    } else {
      val Rule(lhs, rhs) = rule
      val nontermIndexSizeTriples = findRHSCoordinates(rule, size, index)
      //println("Next Indices: "+nontermIndexPairs)

      val words = nontermIndexSizeTriples.map {
        case (nt, (ntsize, i)) =>
          val w = getWordAtIndex(nt, ntsize, i, boundsCheck = false) //we can disable bounds check here as we know the index is within bounds
          w
      }
      if (words.forall(_.isInstanceOf[Element[T]])) {
        var i = -1
        val newword = rhs.foldLeft(List[Terminal[T]]()) {
          case (acc, t: Terminal[T]) => acc :+ t
          case (acc, nt: Nonterminal) =>
            i += 1
            acc ++ words(i).asInstanceOf[Element[T]].word
          case (_, other) =>
            throw new IllegalStateException("Invalid Symbol[T] in rhs: " + other)
        }
        update(ruleYields, rule, size, index, newword)
        Element(newword)
      } else
        //here, we did not find any element. 
        throw new IllegalStateException(
          s"""Did not find any word for $rule Index: $index Size: $size""")
    }
  }

  /**
   * Returns the word belonging to the non-terminal at the specified index.
   * The last parameter is for efficiency.
   */
  def getWordAtIndex(nt: Nonterminal, size: Int, index: BigInt, boundsCheck: Boolean = true): LookupResult[T] = {
    //for stats
    if (gctx.enableStats)
     gctx.stats.updateCounterStats(1, "RecCalls", "WordGenCalls")

    if (opctx.debugGenerator > 1)
      println("Nt: " + nt + " index: " + index)

    val cachedWord = lookup(yields, nt, size, index)
    if (cachedWord.isDefined) {

      //for stats
      /*opctx.stats.updateCounter(1, "HitCalls")
      opctx.stats.updateCounterStats(index.bitLength, "HitIndexSize", "HitCalls")*/

      Element(cachedWord.get)

    } else if (boundsCheck
      && index >= wordCounter.boundForNonterminal(nt, size))
      NoElementAtIndex[T]()
    else {
      //choose a rule for 'nt' based on the size and index      
      val (rule, ruleIndex) = chooseRule(nt, size, index)
      getWordAtIndex(rule, size, ruleIndex) match { //note that size is same as the input size
        case e @ Element(w) =>
          //update the cache 
          update(yields, nt, size, index, w)
          e
        case n @ _ => n
      }
    }
  }

  def findRHSCoordinates(rule: Rule[T], size: Int, index: BigInt) = {
    val Rule(lhs, rhs) = rule
    //get the number of ways of splitting sizes on the right hand symbols
    val alters = wordCounter.possibleSplitsForRule(rule, size)
    if (alters.isEmpty) {
      //this case should never be possible
      throw new IllegalStateException("No way to split the rule: " + rule + " for size: " + size)
    }

    //pick an alternative based on the index
    val (altNumber, altIndex) = chooseAlternative(alters.map(_._2), index) match {
      case None =>
        throw new IllegalStateException(s"""No alternative for (size, index): ($size,$index) """)
      case Some(choice) =>
        /*if (opctx.debugGenerator > 1)
                println(s"""$rule ( $index ) = """ + indices.mkString(" "))*/
        choice
    }

    //compute the indices of the non-terminals in the rule based on the 'altIndex' 
    val nonterms = nontermsInRightSide(rule)
    //computes the nonterminal sizes based on the alternative chosen
    val ntsizes = nonterms match {
      case List() => List()
      case List(nt) => List(alters(altNumber)._1)
      case List(nt1, nt2) =>
        val ntsize1 = alters(altNumber)._1
        val ntsize2 = (size - ntsize1) - (rule.rightSide.size - 2)
        List(ntsize1, ntsize2)
    }
    //compute the bounds on the non-terminals based on the nonterm sizes
    val ntbounds = (nonterms zip ntsizes).map {
      case (nt, m) => wordCounter.boundForNonterminal(nt, m)
    }

    val rhsIndices = chooseProductIndices(ntbounds, altIndex) match {
      case None =>
        throw new IllegalStateException("No inverse mapping for alternative index: " + altIndex)
      case Some(indices) =>
        if (opctx.debugGenerator > 1)
          println(s"""$rule[$size]( $index ) = """ + indices.mkString(" "))
        indices
    }

    //for debugging
    //Alters: $alters Alternative: ${alters(altNumber)} altIndex: $altIndex Ntsizes :$ntsizes NtBounds: $ntbounds RHS index: $rhsIndices

    (nonterms zip (ntsizes zip rhsIndices))
  }

  def chooseRule(nt: Nonterminal, size: Int, index: BigInt) = {
    //choose a rule for 'nt' based on the size and index
    val ntruleBounds = wordCounter.rulesForNonterminal(nt, size)
    if (ntruleBounds.isEmpty) {
      //this case should not be possible
      throw new IllegalStateException(s"""No rules for nonterm $nt[$size]!""")
    }

    val (ruleNum, ruleIndex) = chooseAlternative(ntruleBounds.map(_._2), index) match {
      case None =>
        throw new IllegalStateException(s"""No rule choice defined for (size, index): ($size,$index) for nonterminal $nt""")
      case Some((rnum, rindex)) =>
        if (opctx.debugGenerator > 1)
          println(s"""$nt[$size]($index) = """ + (rnum, rindex))
        (rnum, rindex)
    }
    val rule = ntruleBounds(ruleNum)._1
    (rule, ruleIndex)
  }

  import scala.math._
  //construct a cantor inverse function for every rule
  def chooseProductIndices(productBounds: List[BigInt], z: BigInt) = {
    productBounds match {
      case List() => Some(List()) //note this cannot be none, because of the its usage
      case List(b) =>
        if (z < b)
          Some(List(z))
        else
          None
      case List(xbi, ybi) =>
        val inv = inverseCantor(xbi, ybi, z)

        if (opctx.verifyInverseCantor) {
          inv match {
            case None =>
              //check if z > cantor(xbounds-1,ybounds)
              if (z <= cantor(xbi, ybi, xbi - 1, ybi))
                throw new IllegalStateException(z +
                  "'s inverse is undefined though it contained in the space:(" + (xbi - 1) + "," + ybi)
              else
                None
            case res @ Some(List(x, y)) =>
              if (z != cantor(xbi, ybi, x, y))
                throw new IllegalStateException(z + " not equal to the image of its inverse: " + (x, y))
              else
                res
          }
        } else
          inv
    }
  }

  def inverseCantor(xbi: BigInt, ybi: BigInt, z: BigInt) = {
    require(z >= 0)
    //compute the inverse cantor mapping here    
    val xb = xbi - 1
    val yb = ybi

    //val xexceed = ((xb + 1) * (xb + 2)) / 2
    //val yexceed = (yb * (yb + 1)) / 2
    val xexceed = ((xb + 1) * (xb + 2)) >> 1
    val yexceed = (yb * (yb + 1)) >> 1
    val bothExceed =
      if (xb > yb - 1) {
        //yb * (xb - yb + 1) + (yb * (yb + 1)) / 2
        yb * (xb - yb + 1) + ((yb * (yb + 1)) >> 1)
      } else if (yb - 1 > xb)
        //(xb + 1) * (yb - xb - 1) + (xb + 1) * (xb + 2) / 2
        (xb + 1) * (yb - xb - 1) + (((xb + 1) * (xb + 2)) >> 1)
      else
        yexceed //here, both xexceed and yexceed are equal           

    if (bothExceed <= z) {
      //here both bounds are crossed
      val wb = xb + yb
      val swb = xb * xb + yb * yb
      //compute the term r^2  - swb  + yb - xb but after scaling it by 4
      //that is, (2r)^2  - 4swb + 4yb - 4xb, where r = xb + yb + 1/2
      //val twor = 2 * xb + 2 * yb + 1
      val twor = (xb << 1) + (yb << 1) + 1
      //val subterm = twor * twor - 4 * swb + 4 * yb - 4 * xb
      val subterm = twor * twor - (swb << 2) + (yb << 2) - (xb << 2)
      //val rootterm = subterm - 8 * z
      val rootterm = subterm - (z << 3)
      if (rootterm < 0) {
        //No points to enumerate. In this case, the space is exhausted
        None
      } else {
        //val w = (twor - Util.ceilSqrt(rootterm)) / 2 //note this computes floor of div 2
        val w = (twor - Util.ceilSqrt(rootterm)) >> 1 //note this computes floor of div 2
        //val t = ((2 * wb - 1) * w - w * w - swb + wb) / 2
        val t = (((wb << 1) - 1) * w - w * w - swb + wb) >> 1
        val y = z - t
        val x = w - y
        Some(List(x, y))
      }
    } else if (xexceed <= z) {
      //here x bound is crossed but not y bound
      val xbsq = xb * xb
      //val w = (2 * z + xbsq + xb) / (2 * (xb + 1)) //this computes floor as all of the numbers are positive quantities
      val w = ((z << 1) + xbsq + xb) / ((xb + 1) << 1) //this computes floor as all of the numbers are positive quantities
      //val t = ((2 * w + 1) * xb - xbsq) / 2
      val t = (((w << 1) + 1) * xb - xbsq) >> 1
      val y = z - t
      val x = w - y
      Some(List(x, y))
    } else if (yexceed <= z) {
      val ybsq = yb * yb
      //val w = (2 * z + ybsq - yb) / (2 * yb) //this this compute floor as all of the numbers are positive quantities
      val w = ((z << 1) + ybsq - yb) / (yb << 1) //this this compute floor as all of the numbers are positive quantities
      //val t = ((2 * w + 1) * yb - ybsq) / 2
      val t = (((w << 1) + 1) * yb - ybsq) >> 1
      val y = z - t
      val x = w - y
      Some(List(x, y))
    } else {
      //val w = (Util.sqrt(8 * z + 1) - 1) / 2
      val w = (Util.sqrt((z << 3) + 1) - 1) >> 1
      //val t = (w * (w + 1)) / 2
      val t = (w * (w + 1)) >> 1
      val y = z - t
      val x = w - y
      Some(List(x, y))
    }
  }

  /**
   * Define a forward cantor function to check the value of the inverse cantor
   */
  def cantor(xbi: BigInt, ybi: BigInt, x: BigInt, y: BigInt) = {
    require(x >= 0 && y >= 0)
    //compute the inverse cantor mapping here    
    val xb = xbi - 1
    val yb = ybi
    val w = x + y
    val z = w * (w + 1) / 2 + y
    if (w < xb && w < yb) {
      z
    } else if (w < yb) {
      val xskip = ((w - xb) * (w - xb + 1)) / 2
      z - xskip
    } else if (w < xb) {
      val yskip = ((w - yb) * (w - yb + 1)) / 2
      z - yskip
    } else {
      val xskip = ((w - xb) * (w - xb + 1)) / 2
      val yskip = ((w - yb) * (w - yb + 1)) / 2
      z - xskip - yskip
    }
  }

  //TODO: this can be slight simplified if there is only one alternative
  def chooseAlternative(altbounds: List[BigInt], z: BigInt) = {
    require(z >= 0)

    //assuming that the 'altBounds' are sorted in ascending order of their bounds           
    val n = altbounds.size
    /*val definedBounds = altbounds
    val k = definedBounds.size
    val mk = altbounds(k - 1)
    val msum = definedBounds.sum - mk*/
    val zmaxb = altbounds.sum

    var mi = bigzero
    var i = 0
    var zprime = z
    var break = false

    //println(s"""z: $z zmaxb: $zmaxb Rules: ${rules.mkString(",")} mk: $mk msum: $msum k: $k""")

    if (z >= zmaxb) {
      //z exceeds all bounds, so return None
      None
    } else {
      while (!break && i < n) {
        val miplus1 = altbounds(i)
        val rhs = mi + miplus1 * n - miplus1 * i
        if (zprime < rhs) {
          //we have found a 'k'
          break = true
        } else {
          //we have exceeded this bound, we have to test the next one
          i = i + 1
          zprime -= mi
          mi = miplus1
        }
      }
      if (i == n) {
        //this case should not be possible, so throw an exception
        throw new IllegalStateException(
          "Exceeeding bound should not have reached here, zmaxb is wrongly computed!! " + zmaxb)
      } else {
        val zlast = zprime - (mi * n - mi * (i - 1))
        val altNum = (i + (zlast % (n - i))).toInt
        val altIndex = mi + (zlast / (n - i)) //this will compute floor
        Some(altNum, altIndex)
      }
    }
  }

  /**
   * The following methods are for constructing a parse tree for a given index
   */
  import parsing._
  def constructParseTreeForRule(rule: Rule[T], size: Int, index: BigInt): Option[ParseTree[T]] = {
    //println("Rule[T]: " + rule + " Index: " + index)

    val Rule(lhs, rhs) = rule
    val nontermIndexSizeTriples = findRHSCoordinates(rule, size, index)
    //println("Next Indices: "+nontermIndexPairs)

    val childTrees = nontermIndexSizeTriples.map {
      case (nt, (ntsize, i)) =>
        constructParseTreeForNonterminal(nt, ntsize, i).get
    }
    var i = -1
    val parseTree = PNode(rule, rhs.foldLeft(List[ParseTree[T]]()) {
      case (acc, t: Terminal[T]) => acc :+ PLeaf[T](t)
      case (acc, _) =>
        i += 1
        acc :+ childTrees(i)
    })
    Some(parseTree)
  }

  def constructParseTreeForNonterminal(nt: Nonterminal, size: Int, index: BigInt): Option[ParseTree[T]] = {
    //println("Nt: " + nt + " index: " + index)
    val (rule, rindex) = chooseRule(nt, size, index)
    constructParseTreeForRule(rule, size, rindex)
  }  
  
  private abstract class AbstractSeqEnumerator(domainSize: BigInt, now: Int) extends Iterator[Word[T]] {
    val maxIndex = if (now <= domainSize) now - 1
    				else (domainSize.toInt - 1)
    var index = -1
    //for stats
    val timer = new Stats.Timer()
    
    def wordAtIndex(index: BigInt) : LookupResult[T]

    override def next() = {
      index += 1
      if (index <= maxIndex) {
        //for stats
       gctx.stats.updateCounter(1, "WordGenCalls")
        timer.reset        

        val w = wordAtIndex(index) match {
          case Element(w) => w
          case _ => null
        }
        //for stats
        gctx.stats.updateCounterTime(timer.timeWithoutGC, "time-per-call", "WordGenCalls")
        w
      } else null
    }

    override def hasNext = {
      index < maxIndex
    }
  }
  
  private abstract class AbstractSamplingEnumerator(domainSize: BigInt, nos: Int) extends Iterator[Word[T]] {
    //here the invariant that nos < domainSize holds
    val rand = new java.util.Random()
    val rangeBits =  Math.min(opctx.maxIndexSizeForGeneration, domainSize.bitLength - 1)//domainSize.bitLength / 2
    //println("Bit count: "+domainSize.bitCount +" Bit range: "+domainSize.bitLength)
    var numWords = 0
    //for stats
    val timer = new Stats.Timer()

    /* The following is the precise way of creating random numbers
     * def nextRand(): BigInt = {
      var cand : BigInt = 0
      do {
        cand = new java.math.BigInteger(rangeBits + 1, rand)
      } while (cand >= domainSize)
      cand
    }*/
    
    def wordAtIndex(index: BigInt) : LookupResult[T]

    override def next() = {
      if (numWords < nos) {
        val index = new java.math.BigInteger(rangeBits, rand) //this is a bit of an under-approximation
        //val index = nextRand()
        //for stats
        gctx.stats.updateCounter(1, "WordGenCalls")
        timer.reset

        val w = wordAtIndex(index) match {
          case Element(w) =>
            numWords += 1
            w
          case _ =>
            throw new IllegalStateException("Index: " + index + " exceeds the bound: " + domainSize)
        }
        //for stats        
        gctx.stats.updateCounterTime(timer.timeWithoutGC, "time-per-call", "WordGenCalls")
        w
      } else null
    }

    override def hasNext = {
      numWords < nos
    }
  }

  private class SeqEnumerator(nt: Nonterminal, size: Int, domainSize: BigInt, now: Int) 
  	extends AbstractSeqEnumerator(domainSize, now) {
	 override def wordAtIndex(index: BigInt) = getWordAtIndex(nt, size, index)    
  }

  private class SamplingEnumerator(nt: Nonterminal, size: Int, domainSize: BigInt, nos: Int)
    extends AbstractSamplingEnumerator(domainSize, nos) {
    override def wordAtIndex(index: BigInt) = getWordAtIndex(nt, size, index)
  }     
  
  /**
   * nos - is number of samples
   */
  def getSamplingEnumerator(nt: Nonterminal, size: Int, nos: Int) = {
    val domainSize = wordCounter.boundForNonterminal(nt, size)
    if (nos >= domainSize)
      new SeqEnumerator(nt, size, domainSize, nos)
    else
      new SamplingEnumerator(nt, size, domainSize, nos)
  }

  def getSeqEnumerator(nt: Nonterminal, size: Int, now: Int): Iterator[Word[T]] = {
    val domainSize = wordCounter.boundForNonterminal(nt, size)
    new SeqEnumerator(nt, size, domainSize, now)
  }  
 
  /**
   * Finds the minimum word for a non-terminal subject to the maxsize
   * of the enumerator
   */
  def getMinWord(nt: Nonterminal) : Option[Word[T]]  = {    
    for(i <- 1 to wordSize) {
      if(wordCounter.boundForNonterminal(nt, i) > 0){
        getWordAtIndex(nt, i, 0, false) match { //find  the word at index 0
          case Element(w) => //here an element must be found 
            return Some(w) 
        }
      }
    }
    None
  }
  
  /**
   * The min word generated by an empty sentential form is the
   * empty word.
   */
  def getMinWord(sf: SententialForm[T]) : Option[Word[T]] = {
    sf.foldLeft(Some(List[Terminal[T]]()): Option[Word[T]]){
      case (None, _) => 
        None
      case (Some(acc), nt : Nonterminal) => 
        getMinWord(nt) match {
          case None => None
          case Some(w) => Some(acc ++ w) 
        }
      case (Some(acc), t : Terminal[T]) => 
        Some(acc :+ t)
    }
  }  
}