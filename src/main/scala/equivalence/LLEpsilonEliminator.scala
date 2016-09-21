package equivalence

import grammar._
import utils._
import CFGrammar._

/**
 * An epsilon eliminator  that preserves LL property.
 * For now, we require that the grammar should be LL(1)
 */
object LLEpslionEliminator {

  //creates a non-terminal with the given name
  def freshNonterminal(name: Option[String]): Nonterminal = {
    val ntname = name match {
      case Some(n) => n
      case _ => "N"
    }
    val freshSym = CFGrammar.freshNonterminal((Some(ntname)))
    freshSym
  }

  def removeEpsilonProductions[T](g: Grammar[T]) = {

    val (nullables, first, follow) = GrammarUtils.nullableFirstFollow(g)

    def alpha(sform: SententialForm[T]): List[SententialForm[T]] = {
      val (head :: tail) = sform //sform is never empty
      //the first character is always non-nullable
      val firstchunk = head +: tail.takeWhile(nullables.contains _)
      firstchunk +: (sform.drop(firstchunk.size) match {
        case emp @ List() => List()
        case rest => alpha(rest)
      })
    }

    var sformNonterms = Map[SententialForm[T], Nonterminal]() //mapping from sforms to non-terminals
    def sformToSymbol(sform: SententialForm[T]): Symbol[T] = sform match {
      case List() =>
        throw new IllegalStateException("Empty sentential form!!")
      case List(sym) =>
        sym //reuse 'sym' here
      case _ if sformNonterms.contains(sform) =>
        sformNonterms(sform)
      case _ =>
        //pick the name of the first non-term
        val ntname = sform.collectFirst {
          case nt : Nonterminal => nt.name 
        }
        val freshnt = freshNonterminal(ntname)
        sformNonterms += (sform -> freshnt)
        freshnt
    }

    var newRules = List[Rule[T]]()
    var queue = List[SententialForm[T]](List(g.start)) //initialize to start
    var seenSforms = Set[Nonterminal](g.start) //using the non-term key of sforms here

    while (!queue.isEmpty) {
      val sf = queue.head
      queue = queue.tail
      //a sanity check
      if (sf.distinct.size < sf.size) {
        //a non-terminal repeats
        throw new IllegalStateException("A non-terminal repeats in " + sf)
      }
      val sfnt = sformToSymbol(sf).asInstanceOf[Nonterminal] //this has to be a non-terminal here
      sf match {
        case (head: Nonterminal) :: tail => //head either is start or is a non-nullable non-terminal           
          g.nontermToRules(head).filterNot(_.rightSide.isEmpty).foreach {
            case Rule(_, gamma) =>
              //rule 1
              val newrhs = alpha(gamma ++ tail).foldLeft(List[Symbol[T]]()) {
                (acc, rightSf) =>
                  val rsym = sformToSymbol(rightSf)
                  rsym match {
                    case rnt: Nonterminal if !seenSforms.contains(rnt) =>
                      //add this to the queue
                      queue = queue :+ rightSf
                      seenSforms += rnt
                    case _ => ;
                  }
                  acc :+ rsym
              }
              newRules :+= Rule(sfnt, newrhs)
          }
        case (head: Terminal[T]) :: tail if tail.size > 0 =>
          //rule 3
          newRules :+= Rule(sfnt, List(head))
          //rule 2
          for (i <- 0 until tail.size) {
            val A = tail(i).asInstanceOf[Nonterminal]
            val gamma2 = tail.drop(i + 1)
            //select only non-epsilon rules of A
            g.nontermToRules(A).filterNot(_.rightSide.isEmpty).foreach {
              case Rule(_, gamma) =>
                val newrhs = alpha(gamma ++ gamma2).foldLeft(List[Symbol[T]]()) {
                  (acc, rightSf) =>
                    val rsym = sformToSymbol(rightSf)
                    rsym match {
                      case rnt: Nonterminal if !seenSforms.contains(rnt) =>
                        //add this to the queue
                        queue = queue :+ rightSf
                        seenSforms += rnt
                      case _ => ;
                    }
                    acc :+ rsym
                }
                newRules :+= Rule(sfnt, head +: newrhs)
            }
          }
        case _ =>
          throw new IllegalStateException("Unexpected sentential form in queue: " + sf)
      }
    }
    //add back the epsilon for the start symbol again
    if (nullables(g.start)) {
      Grammar[T](g.start, Rule(g.start, List[Symbol[T]]()) +: newRules)
    } else
      Grammar[T](g.start, newRules)
  }

  /**
   * Remove non-terminals that generate only epsilon.
   */
  def removeEmptyNonterminals[T](igram: Grammar[T]) = {
    var changed = false
    var g = igram
    do {
      changed = false
      val (nullables, first, follow) = GrammarUtils.nullableFirstFollow(g)
      val emptyNonterms = g.nonTerminals.filter(nt => nt != g.start &&
        first(nt).isEmpty && nullables(nt))
      val nrules = g.rules.map {
        case Rule(l, rhs) if rhs.exists(emptyNonterms.contains _) =>
          changed = true
          Rule(l, rhs.filterNot(emptyNonterms.contains _))
        case r @ _ =>
          r
      }
      g = Grammar[T](g.start, nrules)
    } while (changed)
    g
  }

  def removeFirstNullables[T](g: Grammar[T]) = {

    val (nullables, first, follow) = GrammarUtils.nullableFirstFollow(g)

    var nullToNonnull = Map[Nonterminal, Nonterminal]()
    def nonNull(nt: Nonterminal) = {
      nullToNonnull.getOrElse(nt, {
        val nnt = freshNonterminal(Some(nt.name + "-n")) //-n denotes nonnullness
        nullToNonnull += (nt -> nnt)
        nnt
      })
    }

    def makeFstNonnull(rest: List[Symbol[T]]): List[SententialForm[T]] = rest match {
      case (fnt: Nonterminal) :: tail if nullables(fnt) =>
        (nonNull(fnt) +: tail) +: makeFstNonnull(tail)
      case _ =>
        List(rest) //rest could be empty here
    }

    val transRulesMap = g.nontermToRules map {
      case (lhs, rules) =>
        val nrules = rules flatMap {
          case Rule(_, rhs @ (fnt: Nonterminal) :: tail) if nullables(fnt) =>
            makeFstNonnull(rhs).map(Rule(lhs, _))
          case r @ Rule(_, rhs) =>
            List(r)
        }
        (lhs -> nrules.distinct)
    }
    //add rules for nonNull versions
    val newrules = nullables.toList flatMap {
      case nullnt: Nonterminal =>
        val nnt = nonNull(nullnt)
        //add everything except epsilon productions
        transRulesMap(nullnt).collect {
          case Rule(_, rhs) if rhs.size > 0 =>
            Rule(nnt, rhs)
        }
      case sym @ _ =>
        throw new IllegalStateException("A nullable termainal ? " + sym)
    }
    Grammar[T](g.start, (transRulesMap.values.flatten.toList ++ newrules))
  }

  def eliminateEpsilons[T](g: Grammar[T]): Grammar[T] = {
    require(GrammarUtils.isLL1(g))
    
     val dumpGrammar = (title: String) => (g: Grammar[T]) => {
      println(title + " phase: ");
      println("----------------");
      println(g);
      println("=======");
      g
    }

    val transformations = (
      CNFConverter.addStartSymbol[T] _
      andThen CNFConverter.simplify[T]
      andThen removeEmptyNonterminals
      //andThen dumpGrammar("removeEmptyNonterminals")
      //reducing Arity will probably optimize parts of the algorithm
      andThen CNFConverter.reduceArity
      //andThen dumpGrammar("reduceArity")
      andThen removeFirstNullables
      //andThen dumpGrammar("removeFirstNullables")     
      andThen removeEpsilonProductions)

    transformations(g)
  }
}