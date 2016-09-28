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
  require(isIn2NF(G, false))

  lazy val nullables = GrammarUtils.nullables(G)
  //println("Nullables: "+nullables)
  /**
   * A map from nonterminals to the productions in which they are used
   * along with a nullable nonterminal
   */
  lazy val nontermToUsesWithNullables = {
    var usesWithNullable = Map[Nonterminal, ListBuffer[Rule[T]]]()
    G.rules.foreach {
      case r @ Rule(_, List(x: Nonterminal, y: Nonterminal)) =>
        if (nullables(x)) {
          val lst = usesWithNullable.getOrElse(y, {
            val l = new ListBuffer[Rule[T]]()
            usesWithNullable += (y -> l)
            l
          })
          lst += r
        }
        if (nullables(y)) {
          val lst = usesWithNullable.getOrElse(x, {
            val l = new ListBuffer[Rule[T]]()
            usesWithNullable += (x -> l)
            l
          })
          lst += r
        }
      case _ =>
    }
    usesWithNullable
  }
  //println("Nullable uses" + this.nontermToUsesWithNullables.map { case (nt, rls) => nt.toString + "->" + rls.mkString(",") }.mkString("\n"))

  lazy val nontermsToUnitUses = G.rules.filter(r => r.rightSide.size == 1 && r.rightSide(0).isInstanceOf[Nonterminal]).groupBy { case Rule(_, List(x: Nonterminal)) => x }

  //create two mappings from substrings to CYK parse table entry   
  //The mappings are represented as tries for space efficiency
  val parseTableCache = new TrieMap[TerminalWrapper[T], Set[Nonterminal]]()

  def getRules: List[Rule[T]] = G.rules

  def parse(w: List[Terminal[T]])(implicit opctx: GlobalContext): Boolean = {
    opctx.stats.updateCounter(1, "CYKParseCalls")
    val timer = new Stats.Timer()

    val res = parseWithNonterminal(G.start, w.map(x => new TerminalWrapper(x)).toArray[TerminalWrapper[T]])

    opctx.stats.updateCounterTime(timer.timeWithoutGC(), "CYKParseTime", "CYKParseCalls")
    res
  }

  def parseWithSententialForm(sf: SententialForm[T], w: Word[T])(implicit opctx: GlobalContext): Boolean = {

    opctx.stats.updateCounter(1, "CYKParseSenformCalls")
    val timer = new Stats.Timer()
    val termclassStr = w.map(x => new TerminalWrapper(x)).toArray

    def recParse(sfi: Int, sfj: Int, wi: Int, wj: Int): Boolean = {
      if (sfi == sfj) {
        //compute the word between indices wi and wj (inclusive)
        val substr = termclassStr.slice(wi, wj + 1) // drop wi take (wj - wi + 1)
        sf(sfi) match {
          case t: Terminal[T] =>
            //in this case substr should be 't'
            if (substr.size == 1 && substr(0).compare(t))
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
   * A top down version of CYK parsing extended for unit and epsilon productions, if any.
   * This is more efficient for parsing in batch mode
   */
  def parseWithNonterminal(nt: Nonterminal, w: Seq[TerminalWrapper[T]])(implicit opctx: GlobalContext): Boolean = {
    require(isNormalized(G, false)) // we only handle a normalized grammar

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
                d(i)(i) = getRules.collect { case Rule(l, List(t: Terminal[T])) if w(i).compare(t) => l }.toSet //{X | G contains X->w(p)}             
              if (d(j)(j) == null)
                d(j)(j) = getRules.collect { case Rule(l, List(t: Terminal[T])) if w(j).compare(t) => l }.toSet //{X | G contains X->w(p)}            
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
              // check all partitions of i to j that has at least 1 element  
              for (k <- i + 1 to j) {
                for (
                  rule <- getRules if rule.rightSide.size == 2;
                  Rule(x, List(y: Nonterminal, z: Nonterminal)) = rule
                ) if ((d(i)(k - 1) contains y) && (d(k)(j) contains z))
                  nonterms += x
              }
              d(i)(j) = nonterms //epsilonUnitClosure(nonterms)
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
   * A conventional CYK parser implementation.
   * Note it cannot handle epsilon or unit productions.
   */
  def parseBottomUpWithTable(w: Array[TerminalWrapper[T]])(implicit opctx: GlobalContext): (Boolean, Array[Array[Set[Symbol[T]]]]) = {
    require(isNormalized(G, false))

    val N = w.size
    // d(i)(j) contains the set of all non-terminals which can produce the string between i and j (inclusive)
    val d = Array.fill[Set[Symbol[T]]](N, N)(Set())
    val S = G.start
    for (p <- 0 until N) {
      d(p)(p) = getRules.flatMap { //{X | G contains X->w(p)} 
        case Rule(l, List(t: Terminal[T])) if w(p).compare(t) => List(l)
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
  val parseTreeInfoCache = new TrieMap[TerminalWrapper[T], MultiMap[Nonterminal, ParseInfo[T]]]()

  private def computeParseTreeInfo(w: Array[TerminalWrapper[T]])(implicit opctx: GlobalContext) = {
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
        val substr = w.slice(i, j + 1)
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
              var treeInfo = new MultiMap[Nonterminal, ParseInfo[T]]()
              getRules.foreach {
                case rule @ Rule(l, List(t: Terminal[T])) if (w(i).compare(t)) =>
                  treeInfo.addBinding(l, (rule, 0))
                case _ => ;
              }
              epsilonUnitClosure(treeInfo, i, i)
              d(i)(i) = treeInfo
            }
            if (d(j)(j) == null) {
              var treeInfo = new MultiMap[Nonterminal, ParseInfo[T]]()
              getRules.foreach {
                case rule @ Rule(l, List(t: Terminal[T])) if w(j).compare(t) =>
                  treeInfo.addBinding(l, (rule, 0))
                case _ => ;
              }
              epsilonUnitClosure(treeInfo, j, j)
              d(j)(j) = treeInfo
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
            epsilonUnitClosure(treeInfo, i, j)
            // (note: in the presence of epsilon and unit productions, we don't try to retrieve all trees for convenience)            
            d(i)(j) = treeInfo
            //add the computed entry to the cache
            parseTreeInfoCache.addBinding(substr, treeInfo)
        }
      }
    }
    // the following code is required to handle unit and epsilon productions
    def epsilonUnitClosure(treeInfo: MultiMap[Nonterminal, ParseInfo[T]], i: Int, j: Int) {
      //println(s"Init parse entry ($i,$j): "+treeInfo.keys)
      var nonterms = treeInfo.keySet
      var newnonterms = nonterms
      while (!newnonterms.isEmpty) {
        val toprocess = newnonterms
        newnonterms = Set()
        toprocess.foreach { nt: Nonterminal =>
          // handle uses of nt with nullable non-teminals
          nontermToUsesWithNullables.get(nt).foreach {
            _.foreach {
              case r @ Rule(lhs, List(x, y)) if !nonterms(lhs) =>
                // add a new binding
                val firstPartition = if (x == nt) (j - i + 1) else 0
                treeInfo.addBinding(lhs, (r, firstPartition))
                newnonterms += lhs
              case _ =>
            }
          }
          // handle unit uses of nt
          nontermsToUnitUses.get(nt).foreach {
            _.foreach {
              case r @ Rule(lhs, _) if !nonterms(lhs) =>
                // add a new binding                                            
                treeInfo.addBinding(lhs, (r, j - i + 1)) // here there is only one partition
                newnonterms += lhs
              case _ =>
            }
          }
        }
        nonterms ++= newnonterms
      }
      //println("Final parse entry: "+treeInfo.keys)
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
        case Some(rule) => Stream(PNode(rule, Nil))
        case None       => Stream.empty
      }
    else {
      val termclassWord = w.map { t => new TerminalWrapper(t) }.toArray
      val d = computeParseTreeInfo(termclassWord)
      def getCYKParseTree(i: Int, j: Int, nt: Nonterminal): Stream[ParseTree[T]] = {
        //println(s"Retrieving tree for $nt for str: ($i, $j)")
        val termInfo = 
          if (i == j) {
            //handle the i = j case separately, because the info in the parse tree may be for other terminals (if there is sharing across terminals via terminalClass)
            val trule = G.nontermToRules(nt).find {
              _.rightSide match {
                case List(t: Terminal[T]) => termclassWord(i).compare(t)
                case _                    => false
              }
            }
            trule.map(r => Set((r, 0)))
          } else None
        val treeInfo =
          if (termInfo.isDefined) termInfo
          else if (d(i)(j) != null)
            d(i)(j).get(nt)
          else {
            //get the treeInfo from the cache
            val substr = termclassWord.slice(i, j + 1) //drop i take (j - i + 1)
            parseTreeInfoCache.lookup(substr).get.get(nt)
          }
        treeInfo match {
          case Some(choices) if choices.size > 0 =>
            choices.toStream.flatMap {
              case (r @ Rule(_, List(t: Terminal[T])), _) => Stream(PNode(r, List(PLeaf(w(i))))) // note: here add the input character.
              case (r @ Rule(_, List(nt1: Nonterminal, nt2: Nonterminal)), 0) => // here nt1 reduces to epsilon
                getEpsilonDerivation(nt1) #:: getCYKParseTree(i, j, nt2)
              case (r @ Rule(_, List(nt1: Nonterminal, nt2: Nonterminal)), partSize) if partSize == j - i + 1 => // here nt2 reduces to epsilon
                getCYKParseTree(i, j, nt1) ++ Stream(getEpsilonDerivation(nt2))
              case (r @ Rule(_, List(nt1: Nonterminal, nt2: Nonterminal)), partSize) =>
                val leftStream = getCYKParseTree(i, i + partSize - 1, nt1)
                val rightStream = getCYKParseTree(i + partSize, j, nt2)
                // construct a cartesian product stream of left and right
                leftStream.flatMap { left =>
                  rightStream.map { right =>
                    PNode(r, List(left, right))
                  }
                }
              case (r @ Rule(_, List(nt: Nonterminal)), _) => // unit production
                getCYKParseTree(i, j, nt).map{ t => PNode(r, List(t)) }
            }
          case _ =>
            //println("Cannot find tree!")
            Stream.empty
        }
      }
      getCYKParseTree(0, N - 1, nt)
    }
  }

  lazy val nullableRules = G.nontermToRules.collect {
    case (nt, rules) if nullables(nt) => nt -> rules.find(_.rightSide.forall {
      case nt: Nonterminal => nullables(nt)
      case _               => false
    }).get
  }.toMap

  def getEpsilonDerivation(nt: Nonterminal): ParseTree[T] = {
    nullableRules(nt) match {
      case r @ Rule(_, List()) => PNode[T](r, List())
      case r @ Rule(_, rhs)    => PNode[T](r, rhs.map { case nt: Nonterminal => getEpsilonDerivation(nt) })
    }
  }

  def parseWithCYKTrees(w: List[Terminal[T]])(implicit opctx: GlobalContext): Stream[ParseTree[T]] = parseWithCYKTrees(G.start, w)

  /**
   * Cannot detect ambiguities due to epsilon.
   */
  def hasMultipleTrees(w: List[Terminal[T]])(implicit opctx: GlobalContext): Option[(Nonterminal, List[Terminal[T]], Set[(Rule[T], Int)])] = {
    require(isNormalized(G, false))

    val N = w.size
    val S = G.start
    if (N == 0)
      None
    else {
      val termclassWord = w.map { t => new TerminalWrapper(t) }.toArray
      val d = computeParseTreeInfo(termclassWord)

      def findAmbiguousEntry(i: Int, j: Int, nt: Nonterminal): Option[(Nonterminal, List[Terminal[T]], Set[(Rule[T], Int)])] = {
        val treeInfo =
          if (d(i)(j) != null)
            d(i)(j).get(nt)
          else if (i == j) {
            //this case cannot return a ambiguous tree (by CNF property)
            None
          } else {
            //get the treeInfo from the cache
            val substr = termclassWord.slice(i, j + 1)
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
              /*case (r @ Rule(_, List(nt1: Nonterminal, nt2: Nonterminal)), 0) => // here nt1 reduces to epsilon
                findAmbiguousEntry(i, j, nt2)
              case (r @ Rule(_, List(nt1: Nonterminal, nt2: Nonterminal)), partSize) if partSize == j - i + 1 => // here nt2 reduces to epsilon
                findAmbiguousEntry(i, j, nt1)*/
              case (r @ Rule(_, List(nt1: Nonterminal, nt2: Nonterminal)), partSize) =>
                //first check if the left-sub-tree is ambiguous
                findAmbiguousEntry(i, i + partSize - 1, nt1) match {
                  case None =>
                    //check if the left-sub-tree is ambiguous
                    findAmbiguousEntry(i + partSize, j, nt2)
                  case res @ _ =>
                    res
                }
              /*case (r @ Rule(_, List(nt: Nonterminal)), _) =>
                findAmbiguousEntry(i, j, nt)*/
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
      //case l: PLeaf[T] => List(l)      
      case n @ PNode(r @ Rule(nt, List(sent: Terminal[T])), List(PLeaf(inp))) => // For base cases of CNF, try to preserve the sentinal and the input terminals 
        if (CNFConverter.isCNFNonterminal(nt)) {
          List(LeafWithSentinel(inp, sent))
        } else List(n)

      case PNode(r @ Rule(nt, _), children) if CNFConverter.isCNFNonterminal(nt) =>
        //this node should be skipped as this a temporary
        children.flatMap(recoverParseTree)

      case PNode(r: Rule[T], children) =>
        val newChildren = children.flatMap(recoverParseTree)
        //update the rule 'r'
        val newRule = Rule(r.leftSide, newChildren.map {
          case PNode(Rule(l, _), _)        => l
          case LeafWithSentinel(inp, sent) => sent
        })
        val finalTrees = newChildren.map {
          case n: PNode[T]                 => n
          case LeafWithSentinel(inp, sent) => PLeaf(inp)
        }
        List(PNode(newRule, finalTrees))
    }
    recoverParseTree(initTree) match {
      case List(parseTree) =>
        val PNode(Rule(st, rhs), children) = parseTree
        // rename the start symbol to the original start symbol
        val nname = st.name.split("-")(0)
        PNode(Rule(Nonterminal(scala.Symbol(nname)), rhs), children)
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

  def parseBottomUp(s: List[Terminal[T]])(implicit opctx: GlobalContext): Boolean = {
    val termclassWord = s.map { t => new TerminalWrapper(t) }.toArray
    parseBottomUpWithTable(termclassWord)._1
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
          case Some((r@Rule(_, List(terminal: Terminal[T])), k)) => Some(PLeaf.CNF(r, terminal))
          case Some((r@Rule(_, List(nt1: Nonterminal, nt2: Nonterminal)), k)) =>
            (getParseTree(i, k, nt1), getParseTree(i+k+1, j-k-1, nt2)) match {
              case (Some(left), Some(right)) => Some(PNode.CNF(r, left, right))
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
