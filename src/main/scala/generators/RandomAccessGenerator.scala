/*A old implementation now largely defunct
 * package generators

import grammar._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.{ Map => MutableMap, Set => MutableSet }
import CFGrammar._
import grammar.utils._
import scala.annotation.tailrec
import scala.math.BigInt

object RandomAccessGenerator {  
  type Words = List[Word]
  type Context = List[Nonterminal]

  sealed abstract class FailureReason
  object NotEnoughWords extends FailureReason
  object IndexOverFlowed extends FailureReason
  object NoFailure extends FailureReason

  //a three valued result of querying an element at an index 
  sealed abstract class LookupResult
  object NoElementAtIndex extends LookupResult
  case class Element(word: List[Terminal]) extends LookupResult

  val bigone = BigInt(1)
  val bigmone = BigInt(-1)
  val bigzero = BigInt(0)
}

*//**
 * grammar - the grammar should not to have any epsilon productions (except from the
 * start symbol
 * maxWords - number of strings to be generated
 * debug - prints messages that help in debugging when set to true
 *//*
class RandomAccessGenerator(inG: Grammar)(implicit opctx: OperationContext) {
  import RandomAccessGenerator._

  //remove unproductive rules and reduce arity
  import CNFConverter._
  private val grammar = (removeUnproductiveRules _ andThen reduceArity)(inG)

  //bounds on the maximum possible index of the nonterminals. 
  //identified dynamically
  private val nontermBounds = {    
    RandomAccessGeneratorUtil.wordCountOfNonterminals(grammar)
  }
  private val ruleBounds = {
    grammar.rules.collect {
      case rule if rule.rightSide.forall {
        case t: Terminal => true
        case rnt: Nonterminal => nontermBounds.contains(rnt)
      } => (rule -> rule.rightSide.map {
        case t: Terminal => bigone
        case nt: Nonterminal => nontermBounds(nt)
      }.product)
    }.toMap
  }

  //println("Nonterm bounds: "+nontermBounds.mkString("\n")) // +" Rule bounds: "+ruleBounds)
  //System.exit(0)

  val nontermRules = {
    //compute the basecases after sorting the rules by the rule bounds
    val sortRules = grammar.rules.sortWith((rl1, rl2) => {
      val b1 = ruleBounds.get(rl1)
      val b2 = ruleBounds.get(rl2)
      if (b1.isDefined && !b2.isDefined)
        true
      else if (!b1.isDefined && !b2.isDefined)
        false
      else if (b1.isDefined && b1.get < b2.get)
        true
      else false
    })
    val newg = Grammar(grammar.start, sortRules)
    val basecases = RandomAccessGeneratorUtil.basecaseOfNonterminals(newg)
    newg.nontermToRules.map {
      case (nt, rules) =>
        val baseRule = basecases(nt)
        val nrules = if (ruleBounds.get(baseRule) == ruleBounds.get(rules(0))) {
          //here, we can swap base rule and sort rule
          if (baseRule != rules(0))
            (baseRule +: rules.filterNot(_ == baseRule))
          else
            rules
        } else {
          //here, we can leave the rules as such since rules(0) is
          //guaranteed to be bounded and hence acyclic.
          //It doesn't matter that the base-rule is not the first rule
          rules
        }
        throw new IllegalArgumentException(
            "The baseRule: " + baseRule + " is not the rule with the lowest bound: " + rules(0)
              + " baseRule bound: " + ruleBounds.get(baseRule) + " lowest bound: " + ruleBounds.get(rules(0)))
        (nt -> nrules)
    }
  }

  //println("Nonterm Rules: " + nontermRules.map(entry => (entry._1, entry._2.mkString(","))))

  def reduceArity(inG: Grammar): Grammar = {
    Grammar(inG.start, inG.rules.flatMap(reduceArityOfRule))
  }

  *//**
   * This splits the right hand side as 1 and (n-1).
   * This will ensure better fairness esp. while using cantor ordering.
   *//*
  def reduceArityOfRule(rule: Rule): List[Rule] = {
    if (nontermsInRightSide(rule).size > 2) {
      val firstIndex = rule.rightSide.indexWhere(_.isInstanceOf[Nonterminal])
      val secondIndex = rule.rightSide.indexWhere(_.isInstanceOf[Nonterminal], firstIndex + 1)
      val newSym = Nonterminal(Util.freshName())
      val newRSide = rule.rightSide.take(secondIndex) :+ newSym
      val newRule = Rule(rule.leftSide, newRSide)
      val rest = rule.rightSide.drop(secondIndex)
      val otherRules = reduceArityOfRule(Rule(newSym, rest))
      newRule +: otherRules
    } else
      List(rule)
  }

  def reduceArityOfRule(rule: Rule): List[Rule] = {
    val ntsize = nontermsInRightSide(rule).size
    if (ntsize > 2) {
      //divide the right hand side into half 
      val mid = ntsize / 2
      var i = 0
      var firstHalf = List[Symbol]()
      var secondHalf = List[Symbol]()
      rule.rightSide.foreach {
        case nt: Nonterminal if i < mid =>
          i += 1
          firstHalf :+= nt
        case t: Terminal if i < mid =>
          firstHalf :+= t
        case sym @ _ =>
          secondHalf :+= sym
      }
      val lnt = Nonterminal(Util.freshName())
      val rnt = Nonterminal(Util.freshName())
      val newRule = Rule(rule.leftSide, List(lnt, rnt))

      val leftRules = reduceArityOfRule(Rule(lnt, firstHalf))
      val rightRules = reduceArityOfRule(Rule(rnt, secondHalf))

      newRule +: (leftRules ++ rightRules)
    } else
      List(rule)
  }
  //
  //  private val noterminalRules = {
  //    assert(grammar.rules.forall(r => nontermsInRightSide(r).size <= 2))
  //    grammar.rules.groupBy(_.leftSide)
  //  }

  //a set of words generated by each non terminal
  private var yields = grammar.nonTerminals.map(_ -> MutableMap[BigInt, Word]()).toMap

  *//**
   * A mapping from rules to its yields and also the list of indices for each nonterminal
   * such that index i of a nonterminal 'nt' points to the word yields(nt)(i)
   *//*
  private var ruleYields = grammar.rules.map(_ -> MutableMap[BigInt, Word]()).toMap

  *//**
   * TODO: this is limited by the range of integers
   * To be fixed
   *//*
  class Enumerator(nonterm: Nonterminal) {
    private var ruleIndices = {
      val mmap = MutableMap[Rule, BigInt]()
      grammar.rules.foreach {
        case rule => mmap += (rule -> bigmone)
      }
      mmap
    }

    private var rhsIndices = {
      val mmap = MutableMap[Rule, List[BigInt]]()
      grammar.rules.foreach(rule => mmap += (rule -> List.fill(nontermsInRightSide(rule).size)(bigmone)))
      mmap
    }

    private var validRules = {
      nontermRules.map {
        case (nt, rules) =>
          val lb = new ListBuffer[Rule]()
          lb.appendAll(rules)
          (nt -> lb)
      }
    }

    private var nontermIndex = {
      val mmap = MutableMap[Nonterminal, Int]()
      grammar.nonTerminals.foreach {
        case nt => mmap += (nt -> 0)
      }
      mmap
    }

    *//**
     * Increments index pair as per a diagonal order (cantor order)
     * Also handles bounded structure enumeration
     *//*
    def nextIndexPair(x: BigInt, y: BigInt, xbound: Option[BigInt], ybound: Option[BigInt]): Option[List[BigInt]] = {
      //require(x >= 0 && y >= 0)      
      val nextpair = if (x == 0 || (ybound == Some(y + 1))) {
        //if 'x' is zero or 'y' has reached its upper bound, we go the other extreme
        val newx = x + y + 1
        xbound match {
          case Some(b) if newx >= b =>
            val newy = newx - (b - 1)
            if (!ybound.isDefined || ybound.get > newy) //checking bounds for 'newy'
              Some(List(b - 1, newy))
            else
              //we have exhausted the enumeration of all points inside the closed space              
              None
          case _ => Some(List(newx, bigzero))
        }
      } else {
        //we decrement 'x' and increment'y' until 'x' becomes zero or 'y' reaches an upper bound        
        Some(List(x - 1, y + 1))
      }
      nextpair
    }

    def nextIndex(indices: List[BigInt], bounds: List[Option[BigInt]]): Option[List[BigInt]] = indices match {
      case List() => Some(List())
      case List(x) =>
        bounds(0) match {
          case Some(b) if x >= b => Some(List(b))
          case _ => Some(List(x + 1)) //no bounds here or within bounds satisfied
        }
      case List(`bigmone`, `bigmone`) => Some(List(bigzero, bigzero))
      case List(x, y) =>
        nextIndexPair(x, y, bounds(0), bounds(1))
    }

    def getNextWordOfRule(rule: Rule, index: BigInt): LookupResult = {
      val inversePairing = (index: BigInt) => {
        val nonterms = nontermsInRightSide(rule)
        val currentIndices = rhsIndices(rule)
        nextIndex(currentIndices, nonterms.map(nontermBounds.get _)) match {
          case r @ Some(ixs) =>
            //update the cache here
            rhsIndices(rule) = ixs
            r
          case r @ _ =>
            None
        }
      }
      getWordAtIndex(rule, index, inversePairing, getNextWordOfNonterminal _)
    }

    def getNextWordOfNonterminal(nt: Nonterminal, index: BigInt): LookupResult = {
      val ruleChoice = (index: BigInt) => {
        val ntrules = validRules(nt)
        if (ntrules.isEmpty)
          None
        else {
          var rnum = nontermIndex(nt)
          var rule = ntrules(rnum)
          var rindex = ruleIndices(rule) + 1
          //update the rule index 
          ruleIndices(rule) = rindex
          //update the nonterm index
          nontermIndex(nt) = rnum
          if (ruleBounds.contains(rule) && rindex == ruleBounds(rule) - 1) {
            //we have enumerated the last element of this so remove it
            ntrules.remove(rnum)
            //here, keep the nontermIndex unchanged modulo the number of remaining rules
            if (!ntrules.isEmpty)
              nontermIndex(nt) = rnum % ntrules.size
          } else {
            //increment the nontermIndex by 1 modulo the number of the remaining rules 
            nontermIndex(nt) = (rnum + 1) % ntrules.size
          }
          Some(rule, rindex)
        }
      }
      getWordAtIndex(nt, index, ruleChoice, getNextWordOfRule _)
    }

    var currentIndex = -1
    def nextLookup() = {
      currentIndex += 1
      getNextWordOfNonterminal(nonterm, currentIndex)
    }

    def getNextWord(): Option[Word] = {
      nextLookup() match {
        case Element(w) => Some(w)
        case _ => None
      }
    }

  }

  def getEnumerator(nt: Nonterminal) = new Enumerator(nt)

  def getWordAtIndex(rule: Rule, index: BigInt, inversePairing: BigInt => Option[List[BigInt]],
    ntWord: (Nonterminal, BigInt) => LookupResult): LookupResult = {

    if (opctx.debugGenerator > 1)
      println("Rule: " + rule + " Index: " + index)

    val wordCache = ruleYields(rule)
    if (wordCache.contains(index)) {
      Element(wordCache(index))
    } else if (ruleBounds.contains(rule) && index >= ruleBounds(rule))
      NoElementAtIndex
    else {
      rule match {
        case Rule(lhs, rhs) =>
          val nonterms = nontermsInRightSide(rule)
          val rhsIndices = inversePairing(index) match {
            case None =>
              throw new IllegalStateException("No inverse mapping for index: " + index)
            case Some(indices) =>
              if (opctx.debugGenerator > 1)
                println(s"""$rule ( $index ) = """ + indices.mkString(" "))
              indices
          }
          val nontermIndexPairs = (nonterms zip rhsIndices)
          //println("Next Indices: "+nontermIndexPairs)

          val words = nontermIndexPairs.map {
            case (nt, i) =>
              val w = ntWord(nt, i)
              if (opctx.debugGenerator > 1)
                println(nt + "(" + i + ")" + "=" + w)
              w
          }
          if (words.forall(_.isInstanceOf[Element])) {
            var i = -1
            val newword = rhs.foldLeft(List[Terminal]()) {
              case (acc, t: Terminal) => acc :+ t
              case (acc, nt: Nonterminal) =>
                i += 1
                acc ++ words(i).asInstanceOf[Element].word
              case (_, other) =>
                throw new IllegalStateException("Invalid Symbol in rhs: " + other)
            }
            ruleYields(rule) += (index -> newword)
            Element(newword)
          } else
            //here, we did not find any element. 
            throw new IllegalStateException(
              s"""Did not find any word for $rule Index: $index RHS index: $rhsIndices bounds: ${nonterms.map(nontermBounds.get)} word: $words""")
      }
    }
  }

  *//**
   * Returns the word belonging to the non-terminal at the specified index.
   *//*
  def getWordAtIndex(nt: Nonterminal, index: BigInt, ruleChoice: BigInt => Option[(Rule, BigInt)],
    ruleWord: (Rule, BigInt) => LookupResult): LookupResult = {

    if (opctx.debugGenerator > 1)
      println("Nt: " + nt + " index: " + index)

    val wordCache = yields(nt)
    if (wordCache.contains(index)) {
      Element(wordCache(index))
    } else if (nontermBounds.contains(nt) && index >= nontermBounds(nt))
      NoElementAtIndex
    else {
      val (rule, rindex) = ruleChoice(index) match {
        case None =>
          throw new IllegalStateException("No rule choice defined for index: " + index + " for nonterminal: " + nt)
        case Some((rule, rindex)) =>
          if (opctx.debugGenerator > 1)
            println(s"""$nt ( $index ) = """ + (rule, rindex))

          (rule, rindex)
      }
      ruleWord(rule, rindex) match {
        case e @ Element(w) =>
          //update the cache 
          yields(nt) += (index -> w)
          e
        case n @ _ => n
      }
    }
  }

  *//**
   * Generates maxNow unique words for a nonterminal.
   * It enumerates words upto the given maxIndex.
   * If it is not possible to enumerate enough words provides a reason for that
   *//*
  def genWordsForNonterminal(nt: Nonterminal, maxNow: Int): (List[Word], FailureReason) = {
    val maxIndex = opctx.maxIndexForGeneration
    //using mutable collection for efficiency    
    var words = scala.collection.mutable.LinkedHashSet[Word]()
    var index = 0
    var break = false
    val enum = this.getEnumerator(nt)
    while (!break && words.size < maxNow && index <= maxIndex) {
      val lookupRes = enum.nextLookup()
      lookupRes match {
        case Element(w) => words += w
        case _ => break = true
      }
      index += 1
    }
    val reason = if (index > maxIndex) IndexOverFlowed
    else if (words.size < maxNow) NotEnoughWords
    else NoFailure
    (words.toList, reason)
  }

  def genWords(maxNow: Int): (List[Word], FailureReason) = {
    genWordsForNonterminal(grammar.start, maxNow)
  }

  *//**
   * Generate a random word form a set words of size 'min' to 'max'
   *//*
  def genRandomWord(minsize: Int, maxsize: Int): Option[Word] = {
    val maxIndex = opctx.maxIndexForGeneration
    var words = Set[Word]()
    var index = 0
    var break = false
    while (!break && index <= maxIndex) {
      val lookupRes = getWordAtIndex(inG.start, index)
      lookupRes match {
        case Element(w) if (w.size < minsize) =>
          ; //continue
        case Element(w) if (w.size <= maxsize) =>
          words += w
        case _ =>
          break = true
      }
      index += 1
    }
    if (words.isEmpty)
      None
    else {
      //generate a random number from words
      val randomIndex = scala.util.Random.nextInt(words.size)
      Some(words.toList(randomIndex))
    }
  }

  *//**
   * Returns a list of words (without duplicates) generated by the sentential form
   * @param maxNow maximum number of words
   *//*
  def genWordsForSententialForm(sform: List[Symbol], maxNOW: Int): List[Word] = {
    //generate 'maxNOW' unique strings for each symbol in sform and pick
    // the first 'maxNow' words of the cartesian product    
    val listOfWords = sform.map(symb => symb match {
      case t: Terminal => List(List(t))
      case nt: Nonterminal =>
        //generate maxNow unique words 
        val ntwords = genWordsForNonterminal(nt, maxNOW)._1
        ntwords
    })
    if (listOfWords.isEmpty) {
      //nothing can be generated
      List()
    } else {
      val head :: tail = listOfWords
      val words = tail.foldLeft(head)((acc, words) => {
        val product = Util.cartesianProduct(acc, words, Some(maxNOW))
        product.map { case (w1, w2) => w1 ++ w2 }
      })
      words
    }
  }

  import scala.math._
  //construct a cantor inverse function for every rule
  lazy val ruleInverses = {
    grammar.rules.map { rule =>
      val ruleInv = nontermsInRightSide(rule) match {
        case List() => ((x: BigInt) => Some(List())) //note this cannot be none, because of the its usage
        case List(nt) => ((x: BigInt) => {
          if (!nontermBounds.contains(nt) || x < nontermBounds(nt))
            Some(List(x))
          else
            None
        })
        case List(xnt, ynt) =>
          val inv = inverseCantor(xnt, ynt)
          if (opctx.verifyInverseCantor) {
            val f = cantor(xnt, ynt)
            (z: BigInt) => {
              inv(z) match {
                case None =>
                  //check if z > f(xbounds-1,ybounds)
                  if (z <= f(nontermBounds(xnt) - 1, nontermBounds(ynt)))
                    throw new IllegalStateException(z + "'s inverse is undefined though it contained in the space:("
                      + (nontermBounds(xnt) - 1) + "," + nontermBounds(ynt))
                  else
                    None
                case res @ Some(List(x, y)) =>
                  if (z != f(x, y))
                    throw new IllegalStateException(z + " not equal to the image of its inverse: " + (x, y))
                  else
                    res
              }
            }
          } else
            inv
      }
      (rule -> ruleInv)
    }.toMap
  }

  def inverseCantor(xnt: Nonterminal, ynt: Nonterminal) = {
    //compute the inverse cantor mapping here
    val xbi = nontermBounds.get(xnt)
    val ybi = nontermBounds.get(ynt)
    val xbopt = xbi map (_ - 1)
    val ybopt = ybi

    //    def withinXB(a: BigInt) = (!xbopt.isDefined || a < xbopt.get) //note: Here also we must use one less than wb
    //    def withinYB(a: BigInt) = (!ybopt.isDefined || a < ybopt.get) //note: yb is always one more than the actual index

    val xexceed = if (xbopt.isDefined) {
      val xb = xbopt.get
      Some(((xb + 1) * (xb + 2)) / 2)
    } else
      None
    val yexceed = if (ybopt.isDefined) {
      val yb = ybopt.get
      Some((yb * (yb + 1)) / 2)
    } else
      None
    val bothExceed = if (xbopt.isDefined && ybopt.isDefined) {
      val xb = xbopt.get
      val yb = ybopt.get
      if (xb > yb - 1) {
        Some(yb * (xb - yb + 1) + (yb * (yb + 1)) / 2)
      } else if (yb - 1 > xb)
        Some((xb + 1) * (yb - xb - 1) + (xb + 1) * (xb + 2) / 2)
      else
        yexceed //here, both xexceed and yexceed are equal
    } else
      None

    val xbsqopt = xbopt map (xb => xb * xb)
    val ybsqopt = ybopt map (yb => yb * yb)
    val rterms =
      if (xbopt.isDefined && ybopt.isDefined) {
        val xb = xbopt.get
        val yb = ybopt.get
        val swb = xb * xb + yb * yb
        //compute the term r^2  - swb  + yb - xb but after scaling it by 4
        //that is, (2r)^2  - 4swb + 4yb - 4xb, where r = xb + yb + 1/2
        val twor = 2 * xb + 2 * yb + 1
        Some(xb + yb, swb, twor, twor * twor - 4 * swb + 4 * yb - 4 * xb)
      } else None

    (z: BigInt) => {
      require(z >= 0)
      //      if(z == 219){        
      //    	println(s"""w0: $w0 withinXB: ${withinXB(w0)} withinYB: ${withinYB(w0)}""")
      //      }
      //withinXB(w0) && withinYB(w0) Some(List(x0, y0))
      if (bothExceed.isDefined && bothExceed.get <= z) {
        //here both bounds are crossed
        val (wb, swb, twor, subterm) = rterms.get
        val rootterm = subterm - 8 * z
        if (rootterm < 0) {
          //No points to enumerate. In this case, the space is exhausted
          None
        } else {
          val w = (twor - Util.ceilSqrt(rootterm)) / 2 //note this computes floor of div 2
          //val w = Math.floor(r - Math.sqrt(rootterm)).toInt          
          val t = ((2 * wb - 1) * w - w * w - swb + wb) / 2
          val y = z - t
          val x = w - y
          if (z == 219) {
            println(s"""wb: $wb swb: $swb 2r: $twor subterm: $subterm""")
            println(s"""w: $w t: $t""")
          }
          Some(List(x, y))
        }
      } //else if (withinYB(w0)) {
      else if (xexceed.isDefined && xexceed.get <= z) {
        //here x bound is crossed but not y bound
        val xb = xbopt.get
        val xbsq = xbsqopt.get
        val w = (2 * z + xbsq + xb) / (2 * (xb + 1)) //this computes floor as all of the numbers are positive quantities
        val t = ((2 * w + 1) * xb - xbsq) / 2
        val y = z - t
        val x = w - y
        Some(List(x, y))
      } //else if (withinXB(w0)) {
      else if (yexceed.isDefined && yexceed.get <= z) {
        val yb = ybopt.get
        val ybsq = ybsqopt.get
        val w = (2 * z + ybsq - yb) / (2 * yb) //this this compute floor as all of the numbers are positive quantities
        val t = ((2 * w + 1) * yb - ybsq) / 2
        val y = z - t
        val x = w - y
        Some(List(x, y))
      } else {
        val w = (Util.sqrt(8 * z + 1) - 1) / 2
        val t = (w * (w + 1)) / 2
        val y = z - t
        val x = w - y
        Some(List(x, y))
      }
    }
  }

  *//**
   * Define a forward cantor function to check the value of the inverse cantor
   *//*
  def cantor(xnt: Nonterminal, ynt: Nonterminal) = {
    //compute the inverse cantor mapping here
    val xbi = nontermBounds.get(xnt)
    val ybi = nontermBounds.get(ynt)
    val xbopt = xbi map (_ - 1)
    val ybopt = ybi

    def withinXB(a: BigInt) = (!xbopt.isDefined || a < xbopt.get)
    def withinYB(a: BigInt) = (!ybopt.isDefined || a < ybopt.get) //note: yb is one more than the actual index

    (x: BigInt, y: BigInt) => {
      require(x >= 0 && y >= 0)

      val w = x + y
      val z = w * (w + 1) / 2 + y
      if (withinXB(w) && withinYB(w)) {
        z
      } else if (withinYB(w)) {
        val xb = xbopt.get
        val xskip = ((w - xb) * (w - xb + 1)) / 2
        z - xskip
      } else if (withinXB(w)) {
        val yb = ybopt.get
        val yskip = ((w - yb) * (w - yb + 1)) / 2
        z - yskip
      } else {
        val xb = xbopt.get
        val yb = ybopt.get
        val xskip = ((w - xb) * (w - xb + 1)) / 2
        val yskip = ((w - yb) * (w - yb + 1)) / 2
        z - xskip - yskip
      }
    }
  }

  def inverseRuleChoice(rules: List[Rule]) = rules match {
    case List() =>
      throw new IllegalStateException("No rules given!")
    case List(rule) =>
      //in this case there is only one alternative
      (z: BigInt) => {
        if (!ruleBounds.contains(rule) || z < ruleBounds(rule))
          Some((rule, z))
        else None
      }
      case _ =>
      //assuming that the rules are sorted in ascending order of their bounds     
      val rls = rules.toArray
      val n = rules.size
      val bounds = (rules map ruleBounds.get _).toArray

      //the following is for optimization
      val definedBounds = bounds.collect { case Some(b) => b }
      val k = definedBounds.size
      val mk = if (k == 0) bigzero else bounds(k - 1).get
      val msum = if (k == 0) bigzero
      else definedBounds.sum - mk
      val zmaxb = if (k == 0) bigzero
      else msum + mk * n - mk * (k - 1)

      (z: BigInt) => {
        require(z >= 0)

        var mi = bigzero
        var i = 0
        var zprime = z
        var break = false

        //println(s"""z: $z zmaxb: $zmaxb Rules: ${rules.mkString(",")} mk: $mk msum: $msum k: $k""")

        if (z >= zmaxb) {
          //z exceeds all bounds, so we have only rules that produce infinite strings
          i = k
          mi = mk
          zprime = z - msum
        } else {
          while (!break && i < n) {
            if (bounds(i).isDefined) {
              val miplus1 = bounds(i).get
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
            } else {
              //here the rhs is infinity, hence we have found a 'k' here as well
              break = true;
            }
          }
        }
        if (i == n)
          None //here,  we have exceeded all bounds 
        else {
          val zlast = zprime - (mi * n - mi * (i - 1))
          val ruleNum = (i + (zlast % (n - i))).toInt
          val ruleIndex = mi + (zlast / (n - i)) //this will compute floor
          Some(rls(ruleNum), ruleIndex)
        }
      }
  }

  //construct an inverse ruleChoice function for every nonterminal
  lazy val ntInverses = {
    grammar.nonTerminals.map { nt =>
      //nontermRules(nt) is sorted by the bounds of the rules
      val choiceInv = inverseRuleChoice(nontermRules(nt))
      (nt -> choiceInv)
    }.toMap
  }

  def getWordAtIndexRule(rule: Rule, index: BigInt): LookupResult = {
    getWordAtIndex(rule, index, ruleInverses(rule), getWordAtIndexNonterminal _)
  }

  def getWordAtIndexNonterminal(nt: Nonterminal, index: BigInt): LookupResult = {
    getWordAtIndex(nt, index, ntInverses(nt), getWordAtIndexRule _)
  }

  *//**
   * The following methods are for constructing a parse tree for a given index
   *//*
  import parsing._
  def constructParseTreeForRule(rule: Rule, index: BigInt): Option[ParseTree] = {
    //println("Rule: " + rule + " Index: " + index)
    if (ruleBounds.contains(rule) && index >= ruleBounds(rule))
      None
    else {
      rule match {
        case Rule(lhs, rhs) =>
          val nonterms = nontermsInRightSide(rule)
          val rhsIndices = ruleInverses(rule)(index).get
          val nontermIndexPairs = (nonterms zip rhsIndices)
          //println("Next Indices: "+nontermIndexPairs)          
          val childTrees = nontermIndexPairs.map {
            case (nt, i) =>
              constructParseTreeForNonterminal(nt, i).get
          }
          var i = -1
          val parseTree = Node(rule, rhs.foldLeft(List[ParseTree]()) {
            case (acc, t: Terminal) => acc :+ Leaf(t)
            case (acc, _) =>
              i += 1
              acc :+ childTrees(i)
            //              case (_, other) =>
            //                throw new IllegalStateException("Invalid Symbol in rhs: " + other)
          })
          Some(parseTree)
      }
    }
  }

  def constructParseTreeForNonterminal(nt: Nonterminal, index: BigInt): Option[ParseTree] = {
    //println("Nt: " + nt + " index: " + index)
    if (nontermBounds.contains(nt) && index >= nontermBounds(nt))
      None
    else {
      val (rule, rindex) = ntInverses(nt)(index).get
      constructParseTreeForRule(rule, rindex)
    }
  }
}*/