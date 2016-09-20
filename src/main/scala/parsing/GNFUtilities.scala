package parsing

import grammar._
import utils._
import Logging._
import CFGrammar._
import CNFConverter._

/**
 * A converter to Greiback Normal Form GNF
 * Requires grammars to be in CNF
 */
object GNFUtilities {

  /**
   * This method will also handle epsilons
   */
  def indirectLeftRecursiveNonterms[T](g: Grammar[T]) = {
    val nullables = GrammarUtils.nullables(g)
    (new GraphUtil.DirectedGraph[Nonterminal] {
      def start = g.start
      def vertices = g.nonTerminals
      def successors(v: Nonterminal) = {
        g.nontermToRules(v).flatMap { rl =>
          var foundNonnullable = false
          rl.rightSide.foldLeft(List[Nonterminal]()) { (acc, sym) =>
            if (!foundNonnullable) {
              sym match {
                case nt: Nonterminal if nullables(nt) =>
                  acc :+ nt //here nt is nullable, so we need to continue choosing the next non-terminal if any
                case nt: Nonterminal =>
                  foundNonnullable = true
                  acc :+ nt
                case _ =>
                  foundNonnullable = true
                  acc
              }
            } else acc
          }
        }
      }
    }).sccs
  }

  def hasIndirectLeftRecursion[T](g: Grammar[T]) = {
    indirectLeftRecursiveNonterms(g).exists(_.size >= 2)
  }

  /**
   * assuming the grammar is in GNF form
   */
  def firstNT[T](nt: Nonterminal, g: Grammar[T]): List[Terminal[T]] = {
    g.nontermToRules(nt).collect {
      case Rule(_, (t: Terminal[T]) :: tail) => t
      case r @ Rule(_, (nt: Nonterminal) :: tail) =>
        throw new IllegalStateException("Rule: " + r + " is not in GNF form")
    }.distinct
  }

  /**
   * Checks if a grammar in GNF form is LL(2)
   */
  def isGNFGrammarLL2[T](g: Grammar[T]): Boolean = {
    var break = false
    g.nontermToRules.foreach {
      case (nt, ntrules) if !break =>
        val rules = ntrules.filter {
          case Rule(_, head :: tail) => true
          case Rule(l, List()) if l == g.start => false
          case rl @ _ =>
            throw new IllegalStateException("Found epsilon rule for non-start symbol: " + rl)
        }
        val headToRules = rules.groupBy {
          case Rule(_, (head: Terminal[T]) :: tail) => head
          case rl @ _ =>
            throw new IllegalStateException("Rule not in GNF: " + rl)
        }
        headToRules.foreach {
          case (frst, rules) if !break =>
            val seconds = rules.collect {
              case Rule(_, _ :: s :: tail) => s
            }
            //the first set of each of the seconds should be disjoint              
            var firsts = Set[Terminal[T]]()
            seconds.foreach {sym =>
              if (!break) 
                sym match {
                  case t: Terminal[T] if firsts(t) =>
                    break = true
                  case t: Terminal[T] =>
                    firsts += t
                  case nt: Nonterminal =>
                    val ntfirsts = firstNT(nt, g).toSet
                    if (ntfirsts.intersect(firsts).isEmpty)
                      firsts ++= ntfirsts
                    else
                      break = true
                }
            }
          case _ => ; //do nothing
        }
      case _ => ; //fo nothing
    }
    !break
  }  
  
  /**
   * Performs left factorization, not really used anywhere
   */
  def leftFactor[T](g: Grammar[T]): Grammar[T] = {

    def factorOnce(rules: List[Rule[T]]) = {
      var modRules = List[Rule[T]]()
      var newRules = List[Rule[T]]()
      rules.groupBy(_.leftSide).foreach {
        case (nt, rules) =>
          val headToRules = rules.groupBy {
            case Rule(_, List()) => List()
            case Rule(_, head :: tail) => head
          }
          headToRules.foreach {
            case (h: Symbol[T], rules) =>
              val (rulesToFactor, rest) = rules.partition(_.rightSide.size >= 2)
              if (rulesToFactor.size >= 2) {
                //create a new non-terminal to produce the suffix
                val sufNT = Nonterminal(Util.freshName(Some("Suf")))
                val sufRHS = rulesToFactor.map(_.rightSide.drop(1)) //drop the head from the rightsides
                modRules ++= (Rule(nt, List(h, sufNT)) +: rest)
                newRules ++= sufRHS.map(Rule(sufNT, _))
              } else
                modRules ++= rules
            case _ =>
              modRules ++= rules
          }
      }
      (modRules, newRules)
    }
    var rulesToCheck = g.rules
    var rulesTransformed = List[Rule[T]]()
    while (!rulesToCheck.isEmpty) {
      val (modrules, newrules) = factorOnce(rulesToCheck)
      rulesToCheck = newrules
      rulesTransformed ++= modrules
    }
    Grammar[T](g.start, rulesTransformed)
  }

  /**
   * This also will handle epsilons
   */
  /*def removeLeftRecursionOptimized(ntrules: List[Rule], nt: Nonterminal): List[Rule] = {

    def isLeftRecursive(rule: Rule): Boolean = rule match {
      case Rule(lhs, head :: _) if lhs == head => true
      case _ => false
    }

    //substitute for epsilons
    //val afterEpsilons = CNFConverter.removeEpsilonProductions(Grammar[T](nt, ntrules)).rules     
    //collect all left recursive rules
    val (leftRecur, rest) = ntrules.partition(isLeftRecursive)
    if (leftRecur.isEmpty)
      ntrules
    else {
      val alphas = leftRecur.map(_.rightSide.tail)
      val betas = rest.map(_.rightSide)

      //create a new non-terminal for alpha if it has many productions
      val alphaNT = Nonterminal(Util.freshName(Some(nt.name)))
      val alphaRules = alphas.map(Rule(alphaNT, _))
      val betaNT = Nonterminal(Util.freshName(Some(nt.name)))
      val betaRules = betas.map(Rule(betaNT, _))

      val Z = Nonterminal(Util.freshName(Some(nt.name)))
      List(Rule(nt, List(betaNT)), Rule(nt, List(betaNT, Z))) ++
        List(Rule(Z, List(alphaNT, Z)), Rule(Z, List(alphaNT))) ++
        alphaRules ++ betaRules
    }
  }

  */
  /**
   * The following method is not really used by the GNFConverter itself,
   * but by other clients. Eps. by the antlr parser
   */ /*
  def removeIndirectLeftRecursion(g: Grammar[T])(implicit opctx: OperationContext): Grammar[T] = {
    val indirectLeftRecurs = (new GraphUtil.DirectedGraph[Nonterminal] {
      def start = g.start
      def vertices = g.nonTerminals
      def successors(v: Nonterminal) = {
        g.nontermToRules(v).collect {
          case Rule(_, (firstNt: Nonterminal) :: rest) =>
            firstNt
        }
      }
    }).sccs.filter(_.size >= 2)

    val nonterms = g.nonTerminals.toSet
    val newRuls = indirectLeftRecurs.flatMap { scc =>

      println("SCC: " + scc)
      val sccRules = scc.flatMap(g.nontermToRules.apply _)
      println("#old rules : " + sccRules.size)
      println("Old Rules for Scc: " + CFGrammar.rulesToStr(sccRules))

      //order the nonterminals as A1 ... An, arbitrarily              	
      val ntIndexMap = scc.zipWithIndex.toMap
      val leftiesSet = scc.toSet
      //convert every rule A_i -> A_j \alpha so that j >= i
      //assuming that the lhs of every rule in ntrules is nt
      def orderNonterm(nt: Nonterminal, ntrules: List[Rule], ntToRules: Map[Nonterminal, List[Rule]]): (List[Rule], List[Rule]) = {
        //println("normalizing nt: " + nt + " Index: " + ntIndexMap(nt))
        //println("#Rules: "+ntrules.size)

        val firstSymToRules = (ntrules.groupBy {
          case rl @ Rule(_, (fnt: Nonterminal) :: rest) =>
            Some(fnt)
          case rl @ _ =>
            None
        })

        val i = ntIndexMap(nt)
        var ntRules = List[Rule]()
        var newRules = List[Rule]()
        firstSymToRules.foreach {
          //if 'fnt' has a smaller index or if it was created newly
          case (Some(fnt), rls) if (ntIndexMap.contains(fnt) && ntIndexMap(fnt) < i)
            || (!nonterms.contains(fnt)) =>

            //println("RecursiveFirstNT: " + fnt)
            val suffs = rls.map { _.rightSide.tail }
            val Z = Nonterminal(Util.freshName(Some(nt.name)))
            val newrls = suffs.map(Rule(Z, _))
            val inlinedRules = ntToRules(fnt).map {
              case Rule(l, r) =>
                Rule(nt, r :+ Z)
            }
            ntRules ++= inlinedRules
            newRules ++= newrls

          case (_, rls) =>
            ntRules ++= rls
        }
        if (!newRules.isEmpty) {
          //println("ntRules: " + ntRules.mkString("\n"))
          //println("newRules: " + newRules.mkString("\n"))
          //recurse
          val (ntrs, nwrs) = orderNonterm(nt, ntRules, ntToRules)
          (ntrs, nwrs ++ newRules)
        } else
          (ntRules, List())
      }

      val orderedRules = scc.foldLeft(Map[Nonterminal, List[Rule]]()) {
        case (acc, nt) =>
          val ntrules = g.nontermToRules(nt)
          //println("#old NTRules: "+ntrules.size)
          val (newNtRules, rest) = orderNonterm(nt, ntrules, acc)
          val nonLeftRecRules = removeLeftRecursionOptimized(newNtRules, nt)
          //println("New rules: "+CFGrammar.rulesToStr(nonLeftRecRules))         
          (acc ++ (nonLeftRecRules ++ rest).groupBy(_.leftSide))
      }.flatMap(_._2).toList

      println("#new rules : " + orderedRules.size)
      //println("New rules for Scc: " + CFGrammar.rulesToStr(orderedRules))
      orderedRules
    }
    //add the new rules for lefties
    val lefties = indirectLeftRecurs.flatten.toSet
    val transRules = g.rules.filterNot(rl => lefties.contains(rl.leftSide)) ++ newRuls

    //simplify the transformed grammar
    val simplifications = {
      removeUnreachableRules _ andThen
        removeUnproductiveRules
    }
    val transGrammar = Grammar[T](g.start, transRules)
    //require(verifyNoIndirectRecursion(transGrammar))
    transGrammar
  }

*/ }