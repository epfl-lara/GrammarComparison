package grammarcomp

package generators

import grammar._
import grammar.CFGrammar._
import grammar.utils._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.{ Set => MutableSet }
import scala.collection.mutable.HashSet
import utils.Util.MultiSet

object GrammarBoundingHelper {
   def freshBoundedNonterminal(name: Option[String]): Nonterminal = {
    val ntname = name match {
      case Some(n) => n + "-b"
      case _ => "N-b"
    }
    val freshSym = CFGrammar.freshNonterminal(Some(ntname))
    freshSym
  }
  
  def isBoudnedNonterminal(nt: Nonterminal) = {
    if (nt.name.contains("-b")) true
    else false
  }
    
  def getUnboundedNonterminal(nt: Nonterminal) = {
    //remove everything after "-b" from the name of the non-terminal
    val endIndex = nt.name.indexOf("-b")
    if(endIndex >= 0) {
      Nonterminal(scala.Symbol(nt.name.substring(0, endIndex)))  
    } else 
      nt    
  }
  
  import parsing._
  def remapParseTree[T](ptree: ParseTree[T]) : ParseTree[T] = ptree match {
    case PNode(Rule(lhs, rhs), children) =>
      val childTrees = children.map(remapParseTree)
      val newrule = Rule(getUnboundedNonterminal(lhs), rhs.map{ 
        case t : Terminal[T] => t
        case nt : Nonterminal =>
          getUnboundedNonterminal(nt)
      })
      PNode(newrule, childTrees)
    case l : PLeaf[T] => l           
  } 
  
  import CNFConverter._
  def createBoundedGrammar[T](grammar : Grammar[T], cycleLen : Option[Int]) = {
    val pg = grammar
    println("Plain size: #nonterminals = " + pg.nonTerminals.size + " #rules=" + pg.rules.size)
    val g = (removeUnreachableRules[T] _ andThen removeEpsilonProductions andThen removeUnproductiveRules)(pg)
    println("Normalized size: #nonterminals = " + g.nonTerminals.size + " #rules=" + g.rules.size)
    
    val plainbg = (new GrammarBoundingHelper(g)).boundGrammar(1, cycleLen)
    
    println("Plain bg size: #nonterminals = " + plainbg.nonTerminals.size + " #rules=" + plainbg.rules.size)
    
    //remove unreachable and unproductive non-terminals if any
    val bg = (removeUnreachableRules[T] _ andThen removeUnproductiveRules)(plainbg)
    //GrammarWriter.dumpGrammar(boundFilename, bg)
    println("Bounded size: #nonterminals = " + bg.nonTerminals.size + " #rules=" + bg.rules.size)
    bg
  }
}

class GrammarBoundingHelper[T](g: Grammar[T]) {

  import GrammarBoundingHelper._
  val reachSet = {
    var mreach = Map[Nonterminal, MutableSet[Nonterminal]]()
    var changed = true
    //repeat until fixpoint
    while (changed) {
      changed = false
      g.rules.foreach {
        case Rule(lhs, rhs) =>
          //collect the reach set of all the successors
          val succReach = rhs.flatMap {
            case nt: Nonterminal if mreach.contains(nt) =>
              mreach(nt) + nt
            case _ =>
              new HashSet[Nonterminal]()
          }

          if (mreach.contains(lhs)) {
            val diffset = succReach.filterNot(mreach(lhs).contains)
            if (!diffset.isEmpty) {
              //there are new things to add
              mreach(lhs) ++= diffset
              changed = true
            }
          } else {
            val hset = new HashSet[Nonterminal]()
            hset ++= succReach
            mreach += (lhs -> hset)
            changed = true
          }
      }
    }
    val reachMap = mreach.map {
      case (k, v) => (k -> v.toSet)
    }.toMap

    //println("Finished Computing Reachset!!")
    reachMap
  }   

  /**
   * The length of the context is the maximum number of non-terminals that can
   * be kept track in the context.
   * Intuitively, the contextLength specifies the maximum length of the cycle that
   * will be removed. If it is 1, cycles of length <= 1 will be removed,
   * if it is 'n', cycles of length <= n will be removed.
   * TODO: can we use another strategy for this, like the kth non-terminals etc.
   */
  def boundGrammar(recursionDepth: Int, contextLength: Option[Int] = None): Grammar[T] = {

    //a mapping from non-terminal, context to a new non-terminal
    //note that the context could be empty
    var contextMap = Map[(Nonterminal, MultiSet[Nonterminal]), Nonterminal]()
    //var contextMap = Map[(Nonterminal, Set[Nonterminal]), Nonterminal]()
    var newrules = List[Rule[T]]()

    //println("Reach set: "+Util.setString(this.reachSet(Nonterminal("Expression")).toList))
    //    import CNFConverter._
    //    val ng = (removeUnreachableRules _ andThen removeUnproductiveRules)(Grammar(Nonterminal("Expression1"), g.rules))
    //    GrammarWriter.dumpGrammar("temp", ng)
    //    System.exit(0)
    var ntcontext = MultiSet[Nonterminal]() //just for debugging

    def boundRec(lhs: Nonterminal, context: MultiSet[Nonterminal]): Nonterminal = {

      //for debugging
      ntcontext += lhs
      //println("Specializing " + lhs + " # of contexts: " + ntcontext.multiplicity(lhs))

      //create a new non-terminal for the context              
      val freshnt = freshBoundedNonterminal(Some(lhs.name))      
      contextMap += ((lhs, context) -> freshnt)

      //recurse in to the rightsides of 'lhs' using a new context
      val rightSides = CFGrammar.rightSides(lhs, g)
      val newcontext = context prepend lhs
      rightSides.foreach(rhs => {
        //filter RHS' that exceed the recursionDepth
        if (!rhs.exists {
          case nt: Nonterminal if newcontext.multiplicity(nt) >= recursionDepth =>
            //case nt: Nonterminal if newcontext.contains(nt) => 
            true
          case _ => false
        }) {
          val newRHS = rhs.map {
            case nt: Nonterminal =>
              //note that the size of the call-string is bounded by the contextLength and
              //also it will contains only those non-terminals reachable from the rhs symbol
              val callstring = newcontext.contents.filter(this.reachSet(nt).contains)
              val bcs = if (contextLength.isDefined) callstring.take(contextLength.get)
              			else callstring
              val relcontext = MultiSet(bcs)
              //val relcontext = newcontext.intersect(this.reachSet(nt))
              if (contextMap.contains((nt, relcontext))) {
                contextMap((nt, relcontext))
              } else
                boundRec(nt, relcontext)
            case sym => sym
          }
          newrules :+= Rule(freshnt, newRHS)
        }
      })
      freshnt
    }
    val newstart = boundRec(g.start, MultiSet()) //boundRec(g.start, Set())
    val bg = Grammar[T](newstart, newrules)

    //println("# of contexts: "+contextMap.keys.groupBy(_._1).map{ case (k,v) => (k,v.size)})    
    bg
  }
}