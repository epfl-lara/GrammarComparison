package grammarcomp

package parsing
import grammar._
import CFGrammar._
import utils.Util.MultiMap
import utils.Util.TrieMap
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.LinkedHashSet
import scala.collection.mutable.HashSet
import java.io._
import grammar.utils._
import scala.collection.immutable.Range
import ParseTreeUtils._

class CYKParser[T](G: Grammar[T]) extends Parser[T] {
  require(isInCNF(G, false))

  //create two mappings from substrings to CYK parse table entry   
  //The mappings are represented as tries for space efficiency
  val parseTableCache = new TrieMap[Terminal[T], Set[Nonterminal]]()

  def getRules: List[Rule[T]] = G.rules  

  def parse(w: List[Terminal[T]])(implicit opctx: GlobalContext): Boolean = {
    opctx.stats.updateCounter(1, "CYKParseCalls")
    val timer = new Stats.Timer()

    val res = parseWithNonterminal(G.start, w)

    opctx.stats.updateCounterTime(timer.timeWithoutGC(), "CYKParseTime", "CYKParseCalls")
    res
  }

  def parseWithSententialForm(sf: SententialForm[T], w: Word[T])(implicit opctx: GlobalContext): Boolean = {

    opctx.stats.updateCounter(1, "CYKParseSenformCalls")
    val timer = new Stats.Timer()

    def recParse(sfi: Int, sfj: Int, wi: Int, wj: Int): Boolean = {
      if (sfi == sfj) {
        //compute the word between indices wi and wj (inclusive)
        val substr = w drop wi take (wj - wi + 1)
        sf(sfi) match {
          case t: Terminal[T] =>
            //in this case substr should be 't'
            if (List(t) == substr)
              true
            else false
          case nt: Nonterminal =>
            parseWithNonterminal(nt, substr)
        }
      } else {
        val mid = (sfi + sfj) / 2
        //first half of sentential form should parse 
        //one part of the string and the second part 
        //should parse the other
        val res = (wi to wj - 1).exists { i =>
          recParse(sfi, mid, wi, i) &&
            recParse(mid + 1, sfj, i + 1, wj)
        }
        res
      }
    }
    val res = recParse(0, sf.size - 1, 0, w.size - 1)

    opctx.stats.updateCounterTime(timer.timeWithoutGC(), "CYKParseSenformTime", "CYKParseSenformCalls")
    res
  }

  //TODO: will this be fast ??
  def parseWithSententialForms(sfs: SententialForms[T], w: Word[T])(implicit opctx: GlobalContext): Boolean = {
    sfs.exists(parseWithSententialForm(_, w))
  }

  /**
   * A top down version of CYK parsing.
   * This is more efficient for parsing in batch mode
   */
  def parseWithNonterminal(nt: Nonterminal, w: List[Terminal[T]])(implicit opctx: GlobalContext): Boolean = {

    val N = w.size
    if (N == 0)
      getRules.exists(rule => rule.leftSide == nt && rule.rightSide.isEmpty)
    else {
      // d(i)(j) contains the set of all non-terminals which can produce the string between i and j (inclusive)
      val d = Array.ofDim[Set[Nonterminal]](N, N) //caution filling every thing with null

      //a stack used to implement DFS. The entries are substrings of w
      //represented as pairs (i,j) where i is the start index and j is the end index 
      var dfsStack = scala.collection.mutable.Stack[(Int, Int, Boolean)]((0, N - 1, false)) //initialize the stack with the first partition        
      while (!dfsStack.isEmpty) {
        val (i, j, underProcess) = dfsStack.pop
        if (d(i)(j) != null) {
          //nothing to be done here
          ;
        } else {
          //get the substring from i to j (inclusive)
          val substr = w drop i take (j - i + 1)
          val cacheHit = if (!underProcess) {
            //check if we have the substring in the cache          
            parseTableCache.lookup(substr)
          } else None

          cacheHit match {
            case Some(nonterms) =>
              d(i)(j) = nonterms
            case _ if !underProcess =>
              //add the entry again to the stack but with the underProcess flag set
              dfsStack.push((i, j, true))

              //here, we need to create all partitions  and add missing ones to the stack
              //initialize i,i and j,j entries of the matrix
              if (d(i)(i) == null)
                d(i)(i) = getRules.collect { case Rule(l, List(t: Terminal[T])) if compareTerminal(t, w(i)) => l }.toSet //{X | G contains X->w(p)}             
              if (d(j)(j) == null)
                d(j)(j) = getRules.collect { case Rule(l, List(t: Terminal[T])) if compareTerminal(t, w(j)) => l }.toSet //{X | G contains X->w(p)}

              //add missing partitions to the stack
              for (k <- i + 1 to j) {
                if (d(i)(k - 1) == null)
                  dfsStack.push((i, k - 1, false))
                if (d(k)(j) == null)
                  dfsStack.push((k, j, false))
              }
            case _ if underProcess =>
              //here, we have processed all sub-partitions, hence, they all have to be non-null
              var nonterms = Set[Nonterminal]()
              for (k <- i + 1 to j) {
                for (
                  rule <- getRules if rule.rightSide.size == 2;
                  Rule(x, List(y: Nonterminal, z: Nonterminal)) = rule
                ) if ((d(i)(k - 1) contains y) && (d(k)(j) contains z))
                  nonterms += x
              }
              d(i)(j) = nonterms
              //add the computed entry to the cache
              parseTableCache.addBinding(substr, nonterms)
          }
        }
      }
      val res = d(0)(N - 1) contains nt
      res
    }
  }

  /**
   * A conventional CYK parser implementation
   */
  def parseBottomUpWithTable(w: List[Terminal[T]])(implicit opctx: GlobalContext): (Boolean, Array[Array[Set[Symbol[T]]]]) = {

    val N = w.size
    // d(i)(j) contains the set of all non-terminals which can produce the string between i and j (inclusive)
    val d = Array.fill[Set[Symbol[T]]](N, N)(Set())
    val S = G.start
    for (p <- 0 until N) {
      d(p)(p) = getRules.flatMap { //{X | G contains X->w(p)} 
        case Rule(l, List(t: Terminal[T])) if compareTerminal(t, w(p)) => List(l)
        case _ => Nil
      }.toSet
      for (q <- (p + 1) until N) d(p)(q) = Set()
    }
    for (k <- 2 to N) // substring length 
      for (p <- 0 to (N - k)) // initial position 
        for (j <- 1 to (k - 1)) { // length of first half 
          val r = p + j - 1; val q = p + k - 1;
          for (rule <- getRules if rule.rightSide.size == 2; Rule(x, List(y, z)) = rule)
            if ((d(p)(r).contains(y)) && (d(r + 1)(q).contains(z)))
              d(p)(q) = d(p)(q) + x
        }

    if (N > 0) {
      (d(0)(N - 1).contains(S), d)
    } else {
      (getRules.exists(rule => rule.leftSide == S && rule.rightSide.isEmpty), d)
    }
  }

  /**
   * Using the top-down version of CYK parsing to construct the CYK parse tree.
   * The top-down version works better for batch processing
   */
  type ParseInfo[T] = (Rule[T], Int)
  val parseTreeInfoCache = new TrieMap[Terminal[T], MultiMap[Nonterminal, ParseInfo[T]]]()

  //TODO: implement sharing across terminal classes
  def computeParseTreeInfo(w: List[Terminal[T]])(implicit opctx: GlobalContext) = {
    require(w.size > 0)

    val N = w.size
    // d(i)(j) contains the set of all non-terminals which can produce the string between i and j (inclusive)
    val d = Array.ofDim[MultiMap[Nonterminal, ParseInfo[T]]](N, N) //caution filling every thing with null

    //a stack used to implement DFS. The entries are substrings of w
    //represented as pairs (i,j) where i is the start index and j is the end index 
    var dfsStack = scala.collection.mutable.Stack[(Int, Int, Boolean)]((0, N - 1, false)) //initialize the stack with the first partition        
    while (!dfsStack.isEmpty) {
      val (i, j, underProcess) = dfsStack.pop
      if (d(i)(j) != null) {
        //nothing to be done here
        ;
      } else {
        //get the substring from i to j (inclusive)
        val substr = w drop i take (j - i + 1)
        val cacheHit = if (!underProcess) {
          //check if we have the substring in the cache          
          parseTreeInfoCache.lookup(substr)
        } else None

        cacheHit match {
          case Some(nonterms) =>
            d(i)(j) = nonterms
          case _ if !underProcess =>
            //add the entry again to the stack but with the underProcess flag set
            dfsStack.push((i, j, true))

            //here, we need to create all partitions  and add missing ones to the stack
            //initialize i,i and j,j entries of the matrix
            if (d(i)(i) == null) {
              d(i)(i) = new MultiMap[Nonterminal, ParseInfo[T]]()
              getRules.foreach {
                case rule @ Rule(l, List(t: Terminal[T])) if compareTerminal(t, w(i)) =>
                  d(i)(i).addBinding(l, (rule, 0))
                case _ => ;
              }
            }
            if (d(j)(j) == null) {
              d(j)(j) = new MultiMap[Nonterminal, ParseInfo[T]]()
              getRules.foreach {
                case rule @ Rule(l, List(t: Terminal[T])) if compareTerminal(t, w(j)) =>
                  d(j)(j).addBinding(l, (rule, 0))
                case _ => ;
              }
            }
            //add missing partitions to the stack
            for (k <- i + 1 to j) {
              if (d(i)(k - 1) == null)
                dfsStack.push((i, k - 1, false))
              if (d(k)(j) == null)
                dfsStack.push((k, j, false))
            }
          case _ if underProcess =>
            //here, we have processed all sub-partitions, hence, they all have to be non-null
            var treeInfo = new MultiMap[Nonterminal, ParseInfo[T]]()
            for (k <- i + 1 to j) {
              for (
                rule <- getRules if rule.rightSide.size == 2;
                Rule(x, List(y: Nonterminal, z: Nonterminal)) = rule
              ) if ((d(i)(k - 1) contains y) && (d(k)(j) contains z))
                treeInfo.addBinding(x, (rule, k - i))
            }
            d(i)(j) = treeInfo
            //add the computed entry to the cache
            parseTreeInfoCache.addBinding(substr, treeInfo)
        }
      }
    }
    //count the number of null entries in the table (for stats)
    if (opctx.enableStats) {
      //TODO: record these statistics in some stat counters.
      var nullEntries = 0
      for (i <- 0 to N - 1)
        for (j <- i to N - 1)
          if (d(i)(j) == null)
            nullEntries += 1
      val totalEntries = (N * (N + 1)) / 2
      println("Null entry ratio: " + nullEntries.toDouble / totalEntries)
    }
    d
  }

  def parseWithCYKTrees(nt: Nonterminal, w: List[Terminal[T]])(implicit opctx: GlobalContext): Stream[ParseTree[T]] = {
    val N = w.size
    if (N == 0)
      G.nontermToRules(nt).find(_.rightSide.isEmpty) match {
        case Some(rule) => Stream(Node(rule, Nil))
        case None       => Stream.empty
      }
    else {
      val d = computeParseTreeInfo(w)
      def getCYKParseTree(i: Int, j: Int, nt: Nonterminal): Stream[ParseTree[T]] = {
        val treeInfo =
          if (i == j) {
            //handle the i = j case separately, because the info in the parse tree may be for other identifiers (if there is sharing across identifiers)
            val trule = G.nontermToRules(nt).find {
              _.rightSide match {
                case List(t: Terminal[T]) => compareTerminal(t, w(i))
                case _                    => false
              }
            }
            trule.map(r => Set((r, 0)))
          } else if (d(i)(j) != null)
            d(i)(j).get(nt)
          else {
            //get the treeInfo from the cache
            val substr = w drop i take (j - i + 1)
            parseTreeInfoCache.lookup(substr).get.get(nt)
          }
        treeInfo match {
          case Some(choices) if choices.size > 0 =>
            choices.toStream.flatMap {
              case (r @ Rule(_, List(t: Terminal[T])), _) => Stream(Node(r, List(Leaf(w(i))))) // note: here add the input string.
              case (r @ Rule(_, List(nt1: Nonterminal, nt2: Nonterminal)), partSize) =>
                val leftStream = getCYKParseTree(i, i + partSize - 1, nt1)
                val rightStream = getCYKParseTree(i + partSize, j, nt2)
                // construct a cartesian product stream of left and right
                leftStream.flatMap { left =>
                  rightStream.map { right =>
                    Node(r, List(left, right))
                  }
                }
            }
          /*choices.head match {
              case (r @ Rule(_, List(t: Terminal[T])), _) => Some(Node(r, List(Leaf(t))))
              case (r @ Rule(_, List(nt1: Nonterminal, nt2: Nonterminal)), partSize) =>
                (getCYKParseTree(i, i + partSize - 1, nt1), getCYKParseTree(i + partSize, j, nt2)) match {
                  case (Some(left), Some(right)) => Some(Node(r, List(left, right)))
                  case _ => None
                }
            }*/
          case _ => Stream.empty
        }
      }
      getCYKParseTree(0, N - 1, nt)
    }
  }

  def parseWithCYKTrees(w: List[Terminal[T]])(implicit opctx: GlobalContext): Stream[ParseTree[T]] = parseWithCYKTrees(G.start, w)

  def hasMultipleTrees(w: List[Terminal[T]])(implicit opctx: GlobalContext): Option[(Nonterminal, List[Terminal[T]], Set[(Rule[T], Int)])] = {

    val N = w.size
    val S = G.start
    if (N == 0)
      None
    else {
      val d = computeParseTreeInfo(w)

      def findAmbiguousEntry(i: Int, j: Int, nt: Nonterminal): Option[(Nonterminal, List[Terminal[T]], Set[(Rule[T], Int)])] = {
        val treeInfo =
          if (d(i)(j) != null)
            d(i)(j).get(nt)
          else if (i == j) {
            //this case cannot return a ambiguous tree (by CNF property)
            None
          } else {
            //get the treeInfo from the cache
            val substr = w drop i take (j - i + 1)
            parseTreeInfoCache.lookup(substr).get.get(nt)
          }
        treeInfo match {
          case Some(choices) if choices.size > 1 =>
            //found an ambiguous nonterminal
            val substr = w drop i take (j - i + 1)
            Some((nt, substr, choices.toSet))

          case Some(choices) =>
            //here, there is exactly one choice            
            choices.head match {
              case (r @ Rule(_, List(t: Terminal[T])), _) => None
              case (r @ Rule(_, List(nt1: Nonterminal, nt2: Nonterminal)), partSize) =>
                //first check if the left-sub-tree is ambiguous
                findAmbiguousEntry(i, i + partSize - 1, nt1) match {
                  case None =>
                    //check if the left-sub-tree is ambiguous
                    findAmbiguousEntry(i + partSize, j, nt2)
                  case res @ _ =>
                    res
                }
            }
          case _ => None
        }
      }
      findAmbiguousEntry(0, N - 1, S)
    }
  }

  /**
   * Removes the non-terminals introduced due to CNF Conversion
   */
  def cykToGrammarTree(initTree: ParseTree[T])(implicit opctx: GlobalContext): ParseTree[T] = {

    // We a new leaf to allow storing a sentinel value.
    case class LeafWithSentinel[T](input: Terminal[T], sentinel: Terminal[T]) extends ParseTree[T]

    def recoverParseTree(cnfTree: ParseTree[T]): List[ParseTree[T]] = cnfTree match {
      //case l: Leaf[T] => List(l)      
      case n @ Node(r @ Rule(nt, List(sent: Terminal[T])), List(Leaf(inp))) => // For base cases of CNF, try to preserve the sentinal and the input terminals 
        if (CNFConverter.isCNFNonterminal(nt)) {
          List(LeafWithSentinel(inp, sent))
        } else List(n)

      case Node(r @ Rule(nt, _), children) if CNFConverter.isCNFNonterminal(nt) =>
        //this node should be skipped as this a temporary
        children.flatMap(recoverParseTree)

      case Node(r: Rule[T], children) =>
        val newChildren = children.flatMap(recoverParseTree)
        //update the rule 'r'
        val newRule = Rule(r.leftSide, newChildren.map {
          case Node(Rule(l, _), _)         => l
          case LeafWithSentinel(inp, sent) => sent
        })        
        val finalTrees = newChildren.map {
          case n: Node[T] => n
          case LeafWithSentinel(inp, sent) => Leaf(inp) 
        }
        List(Node(newRule, finalTrees))
    }
    recoverParseTree(initTree) match {
      case List(parseTree) =>
        val Node(Rule(st, rhs), children) = parseTree
        // rename the start symbol to the original start symbol
        val nname = st.name.split("-")(0)
        Node(Rule(Nonterminal(scala.Symbol(nname)), rhs), children)
      case _ =>
        throw new IllegalStateException("Root contains a list of parse trees")
    }
  }

  def parseWithTree(s: List[Terminal[T]])(implicit opctx: GlobalContext): Option[ParseTree[T]] = {
    parseWithCYKTrees(s) match {
      case cnftree #:: tail => Some(cykToGrammarTree(cnftree))
      case _                => None
    }
  }

  def parseWithTrees(s: List[Terminal[T]])(implicit opctx: GlobalContext): Stream[ParseTree[T]] = {
    parseWithCYKTrees(s).map { t => cykToGrammarTree(t) }
  }
}

/**
 * Parser from Wikpedia
 * Written and Maintained by Mikael
 */
/*object CYKWikipediaParser extends Parser {
  import CYKParser._
  def parse(g: Grammar, a: List[Terminal[T]])(implicit opctx: OperationContext): Boolean = {
    checkCNF(g)

    //check if 'a' is epsilon and handle it specially
    if (a.isEmpty) {
      if (g.rules.contains(Rule(g.start, List())))
        true
      else false
    } else {

      val n = a.size
      val nonTerminals = g.rules.map(_.leftSide).distinct
      val r = nonTerminals.size
      val startSymbol = g.start
      val ruleStart = g.rules.filter(_.leftSide == startSymbol)
      // P(i, j, a) is true means that the non-terminal indexed by "a" can derive the subword starting at char "i" and ending at char "i+j" included
      val P = Array.fill[Boolean](n, n, r)(false)
      for (i <- 0 to (n - 1)) {
        for (
          rule <- g.rules if rule.rightSide == List(a(i));
          j = nonTerminals.indexOf(rule.leftSide)
        ) {
          P(i)(0)(j) = true
        }
      }
      for (i <- 2 to n) // Length of span
        for (j <- 0 until (n - i + 1)) // Start of span
          for (k <- 1 to (i - 1)) // Partition of span
            for (
              rule <- g.rules if rule.rightSide.length == 2;
              a = nonTerminals.indexOf(rule.leftSide);
              b = nonTerminals.indexOf(rule.rightSide(0));
              c = nonTerminals.indexOf(rule.rightSide(1))
            ) if (P(j)(k - 1)(b) && P(j + k)(i - k - 1)(c)) P(j)(i - 1)(a) = true

      if (List(startSymbol).map(nonTerminals.indexOf(_)).exists(x => (x != -1) && P(0)(n - 1)(x))) {
        true
      } else {
        false
      }
    }
  }
  
  def parseWithTree(g: Grammar, a: List[Terminal[T]])(implicit opctx: OperationContext): Option[ParseTree] = {
    checkCNF(g)       

    val n = a.size
    val nonTerminals = g.rules.map(_.leftSide).distinct
    val r = nonTerminals.size
    val startSymbol = g.start
    val ruleStart = g.rules.filter(_.leftSide == startSymbol)
		val P = Array.fill[Option[(Rule, Int)]](n, n, r)(None) // The int stores the split, if any
		for(i <- 0 to (n-1)) {
		  for(rule <- g.rules if rule.rightSide == List(a(i));
		      j = nonTerminals.indexOf(rule.leftSide)) {
		    P(i)(0)(j) = Some((rule, 0))
		  }
		}
    // P(i, j, a) if non-empty contains a (rule, k) A -> B C where a is the index of "A" which derives (i,i+j) included, and B can derive (i, i+k) and C can derive (i+k+1, i+j) 
    for (i <- 2 to n) // Length of span
      for (j <- 0 until (n - i + 1)) // Start of span
        for (k <- 1 to (i - 1)) // Partition of span
          for (
            rule <- g.rules if rule.rightSide.length == 2;
            a = nonTerminals.indexOf(rule.leftSide);
            b = nonTerminals.indexOf(rule.rightSide(0));
            c = nonTerminals.indexOf(rule.rightSide(1))
          ) {
            if(b == -1)
            	println("Rule: "+rule)
            if (P(j)(k - 1)(b).nonEmpty && 
                P(j + k)(i - k - 1)(c).nonEmpty)
            P(j)(i - 1)(a) = Some((rule, k - 1))
          }

    if(List(startSymbol).map(nonTerminals.indexOf(_)).exists( x => (x != -1) && P(0)(n-1)(x).nonEmpty)) {
      val startX = nonTerminals.indexOf(startSymbol)
      def getParseTree(i: Int, j: Int, t: Nonterminal): Option[ParseTree] = {
        val a = nonTerminals.indexOf(t)
        P(i)(j)(a) match {
          case Some((r@Rule(_, List(terminal: Terminal[T])), k)) => Some(Leaf.CNF(r, terminal))
          case Some((r@Rule(_, List(nt1: Nonterminal, nt2: Nonterminal)), k)) =>
            (getParseTree(i, k, nt1), getParseTree(i+k+1, j-k-1, nt2)) match {
              case (Some(left), Some(right)) => Some(Node.CNF(r, left, right))
              case _ => None
            }
          case _ => None
        }
      }
      getParseTree(0, n-1, g.start)
    } else {
      None
    }
  }
}*/
