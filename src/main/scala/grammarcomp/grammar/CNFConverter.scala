package grammarcomp

package grammar

import utils._
import CFGrammar._

object CNFConverter {
      
  //creates a fresh CNF termporary starting with the given name
  def freshNonterminal(name: Option[String]): Nonterminal = {
    val ntname = name match {
      case Some(n) => n + "-c"
      case _ => "N-c"
    }
    val freshSym = CFGrammar.freshNonterminal(Some(ntname))  
    freshSym
  }
  
  //creates a variable representing a given terminal
  def terminalVariable : Nonterminal = {
    freshNonterminal(Some("t"))
  }    

  def isCNFNonterminal(nt: Nonterminal) =
    if (nt.name.contains("-c"))
      true
    else false
    
  def isTermVariable(nt: Nonterminal) =
    if (nt.name.startsWith("t-c"))
      true
    else false

  //TODO: the use of 'Set' introduces some amount of nondeterminism
  def removeUnitProductions[T](grammar: Grammar[T]): Grammar[T] = {

    def isUnitRule(rule: Rule[T]) = rule match {
      case Rule(_, List(n : Nonterminal)) => true
      case _ => false
    }

    val (unitRules, nonUnitRules) = grammar.rules partition isUnitRule
    //pairs of left and right sides of an unit rule
    val unitRulePairs = unitRules.collect {
      case Rule(left, List(right : Nonterminal)) if left != right => (left, right)
    }

    val newrules = nonUnitRules flatMap {
      case rule @ Rule(leftSide, rightSide) =>
        //println("Processing rule: "+rule)
        //get all the left sides using a fix point
        val otherLeftSides = Util.fixpoint((nonterminalSet: Set[Nonterminal]) => {
          val newLeftSides = unitRulePairs.collect {
            case (k, v) if (nonterminalSet contains v) => k
          }.toSet
          newLeftSides ++ nonterminalSet
        })(Set(leftSide))

        //println("Other left sides: "+otherLeftSides)
        otherLeftSides.map(left => Rule[T](left, rightSide))
    }
    val rulesWOUnitProductions = (nonUnitRules ++ newrules).distinct //drop all the unitRules       
    Grammar[T](grammar.start, rulesWOUnitProductions)
  }

  /**
   * Ensures that the right side of every rule has at most 2 Nonterminals
   */
  def reduceArity[T](grammar: Grammar[T]): Grammar[T] = {

    val rulesWithArityTwo = grammar.rules.flatMap {
      case rule @ Rule(leftSide, rightSide) if rightSide.size <= 2 =>
        List(rule)
      case rule @ Rule(leftSide, rightSide) => {
        //say the rightSide is x0 ... xn
        //create n - 2 fresh nonterminals, A1 ... An-2
        val freshNonterminals = for (i <- 1 to rightSide.size - 2) yield {
          val res = freshNonterminal(Some(leftSide.name))
          res
        }

        //collection of nonterminals that will used to split the rule: A0 (left) -> A1, ...,An-2
        val nonterms = leftSide +: freshNonterminals.toList
        val lastIndex = nonterms.size - 1
        val newrules = {
          for (i <- 0 to lastIndex - 1)
            yield Rule(nonterms(i), List(rightSide(i), nonterms(i + 1))) //Ai -> xi Ai+1

        }.toList :+ Rule(nonterms(lastIndex), rightSide.takeRight(2)) //An-2 -> xn-1 xn 
        newrules
      }
    }
    Grammar[T](grammar.start, rulesWithArityTwo)
  }

  /**
   * Ensures that the terminals occur alone on the right-hand side
   */
  def normalizeTerminals[T](grammar: Grammar[T]): Grammar[T] = {
    //A mapping from a terminal to the nonterminal that produces it      
    var terminalToNonterminal = Map[Terminal[T], Nonterminal]()

    val rulesWithLoneTerminals = grammar.rules flatMap {
      //right sides are all nonterminals
      case rule @ Rule(_, rightSide) if rightSide.forall(_.isInstanceOf[Nonterminal]) =>
        List(rule)
      //right side has a lone terminal
      case rule @ Rule(_, List(t: Terminal[T])) =>
        List(rule)
      //right side has both terminals and nonterminals
      case rule @ Rule(leftSide, rightSide) =>
        //create new rules for the terminals in the rightSide that have not been seen before 
        val newRules = rightSide collect {
          case t: Terminal[T] if !terminalToNonterminal.contains(t) =>
            val freshNonterm = terminalVariable
            terminalToNonterminal += (t -> freshNonterm)
            Rule(freshNonterm, List(t))
        }
        //replace the terminals in the rightSide by the nonterminal that produces them
        val newRightSide = rightSide map {
          case nt: Nonterminal => nt
          case t: Terminal[T] => terminalToNonterminal(t)
        }
        Rule[T](leftSide, newRightSide) +: newRules
    }
    Grammar[T](grammar.start, rulesWithLoneTerminals)
  }

  /**
   * Removes all rules unreachable from the start symbol
   */
  def removeUnreachableRules[T](grammar: Grammar[T]): Grammar[T] = {
    //Here, a set is used but this should not result in nondeterminism
    /*val reachableNonterminals = Util.fixpoint((reach: Set[Nonterminal]) => {
      val newreach = grammar.rules flatMap {
        case Rule(left, right) if reach contains left =>
          right.collect { case nt: Nonterminal => nt }
        case _ => List()
      }
      reach ++ newreach
    })(Set(grammar.start))*/
    val reachableNonterminals = grammar.nontermsInPostOrder.toSet
    val reachableRules = grammar.rules.filter(rule => reachableNonterminals.contains(rule.leftSide))
    Grammar[T](grammar.start, reachableRules)
  }

  /**
   * Removes all epsilon production except Start -> epsilon from the grammar.
   * It also ensures that the only way an empty string can be derived is
   * using Start -> epsilon.
   */
  def removeEpsilonProductions[T](grammar: Grammar[T]): Grammar[T] = {

    def isEpsilonRule(rule: Rule[T]) = rule.rightSide.isEmpty

    /**
     * Generates all possible right sides from the given 'rightSide' by enumerating the cases
     * where the symbols given by 'epsilonSymbols' produce epsilon and does not produce epsilon
     */
    def applyEpsilon(rightSide: List[Symbol[T]], epsilonSymbols: List[Symbol[T]]): List[List[Symbol[T]]] = {
      //find the index of the first symbol that can produce epsilon
      val index = rightSide.indexWhere(epsilonSymbols.contains _, 0)
      if (index >= 0) {
        val prefix = rightSide.take(index) //all elements before the 'index'        
        val afterIndex = rightSide.drop(index + 1) //this will also drop the element at the 'index'        
        val suffixes = applyEpsilon(afterIndex, epsilonSymbols)
        //Consider the cases where 'rightSide(index)' is epsilon and is not epsilon 
        val isEpsilon = suffixes.map(suffix => prefix ++ suffix)
        val notEpsilon = suffixes.map(suffix => (prefix :+ rightSide(index)) ++ suffix)
        isEpsilon ++ notEpsilon
      } else
        List(rightSide)
    }

    var appliedEpsilonRules = Set[Rule[T]]() //tracks all the epsilon rules applied
    def eliminateEpsilonRules(rules: List[Rule[T]]): List[Rule[T]] = {

      val (epsilonRules, nonEpsilonRules) = rules partition isEpsilonRule
      //println("Epsilon Rules: "+epsilonRules)
      val seenRules = appliedEpsilonRules ++ rules
      val newrules = nonEpsilonRules flatMap {
        case rule @ Rule(leftSide, rightSide) =>
          //println("Reducing rule: "+rule)
          val newRightSides = applyEpsilon(rightSide, epsilonRules.map(_.leftSide))
          newRightSides.map(newRight => Rule(leftSide, newRight)).filterNot(seenRules.contains)
      }
      appliedEpsilonRules ++= epsilonRules
      nonEpsilonRules ++ newrules //drop all the epsilonRules      
    }

    //eliminate epsilon productions until none exists 
    val rulesWOEpsilonProductions = Util.repeatUntil(eliminateEpsilonRules,
      (rules: List[Rule[T]]) => rules.filter(isEpsilonRule).isEmpty)(grammar.rules)

    //If Start -> Epsilon was encountered then add it to the rules
    val startProducesEpsilon = Rule(grammar.start, List[Symbol[T]]())
    val newrules = rulesWOEpsilonProductions.distinct ++ {
      if (appliedEpsilonRules.contains(startProducesEpsilon))
        List(startProducesEpsilon)
      else
        List()
    }

    Grammar[T](grammar.start, newrules)
  }

  /**
   * Removes all rules that unproductive i.e, will never produce a valid string
   * consisting only of terminals
   */
  def removeUnproductiveRules[T](grammar: Grammar[T]): Grammar[T] = {
    //Here, a set is used but this should not result in nondeterminism
    val productiveNonterminals = Util.fixpoint((productives: Set[Symbol[T]]) => {
      val newproductives = grammar.rules collect {
        case Rule(left, right) if right.forall(sym => sym.isInstanceOf[Terminal[T]] || productives.contains(sym)) =>
          left
      }
      productives ++ newproductives
    })(Set())

    val productiveRules = grammar.rules.filter(rule =>
      rule.rightSide.forall(sym => sym.isInstanceOf[Terminal[T]] || productiveNonterminals.contains(sym)))
    Grammar[T](grammar.start, productiveRules)
  }

  /**
   * Pulls the rule corresponding to the start symbol to the top if it is not the case
   */
  def pullStartToTop[T](grammar: Grammar[T]): Grammar[T] = {
    val rules = grammar.rules
    if (rules.isEmpty)
      grammar
    else {
      val newRules = if (rules(0).leftSide != grammar.start) {
        val startRule = rules.find(_.leftSide == grammar.start)
        if (startRule.isDefined) {
          startRule.get +: rules.filterNot(_ == startRule.get)
        } else
          throw new IllegalStateException("No rule for start symbol!")
      } else
        rules
      Grammar[T](grammar.start, newRules)
    }
  }

  /**
   * Add a new start symbol if it is used in the right side of productions
   */
  def addStartSymbol[T](g: Grammar[T]): Grammar[T] = {
    if (g.rules.exists(_.rightSide.contains(g.start))) {
      val newstart = copy(g.start)
      Grammar[T](newstart, Rule(newstart, List[Symbol[T]](g.start)) +: g.rules)
    } else
      g
  }

  def toCNF[T](grammar: Grammar[T]): Grammar[T] = {
    val dumpGrammar = (title: String) => (g: Grammar[T]) => {
      println(title + " phase: ");
      println("----------------");
      println(g);
      println("=======");
      g
    }

    val transformations = ( //dumpGrammar("") andThen
      addStartSymbol[T] _
      andThen normalizeTerminals
      //andThen dumpGrammar("Normalize Terminals")
      andThen removeUnproductiveRules
      //andThen dumpGrammar("Unproductive Rules")    						    						
      andThen reduceArity
      //andThen dumpGrammar("Reducing Arity")
      andThen removeEpsilonProductions
      //andThen dumpGrammar("Epsilon Productions")
      andThen removeUnitProductions
      //andThen dumpGrammar("Unit Productions")
      andThen simplify
      //andThen dumpGrammar("Remove Unreachables")
      andThen pullStartToTop)

    transformations(grammar)
  }
  
  /**
   * Removes productions of the form S -> S
   */  
  def removeSelfProductions[T](g: Grammar[T]) : Grammar[T] = {
    Grammar[T](g.start, g.rules.filterNot{
      case Rule(l, List(sym)) => l == sym
      case _ => false
    })
  }
  
  /**
   * This applies removeUnitProductions and
   * removeUnreachableRules until a fix-point
   */
  def simplify[T](ing: Grammar[T]) = {
    val g = removeSelfProductions(ing)
    //it suffices to compare rule sizes as rules can only be removed by the transformation
    val trans = (removeUnproductiveRules[T] _ andThen removeUnreachableRules)
    var currSize = g.rules.size
    var oldSize = 0
    var currg = g
    do {
      oldSize = currSize
      currg = trans(currg)
      currSize = currg.rules.size
    } while(currSize != oldSize)     
    currg
  }

  /**
   * Removes from 'grammar' and 'rules' the temporary symbols created during the conversion to CNF form
   * by inlining them with their right hand sides.
   * Additionally, it also remove unit nonterminals, which are nonterminals
   * that have only one production.
   */
  def removeCNFNonterminals[T](inG: Grammar[T], irules: List[Rule[T]] = List()): List[Rule[T]] = {

    val cnfNonterminals = inG.nonTerminals.filter(isCNFNonterminal).toSet    

    def nontermsInRules(rules: List[Rule[T]]) = {
      rules.flatMap {
        case Rule(ls, rs) =>
          ls +: rs.collect {
            case nt: Nonterminal => nt
            //if cnfNonterminals.contains(nt) => nt
          }
      }.toSet
    }

    def removeCNFNontermsInRules(input: (Grammar[T], List[Rule[T]])) = {
      val (g, rules) = input
      //val nontermsToRemove = nontermsInRules(rules).intersect(cnfNonterminals ++ unitNts)
      val nontermsToRemove = nontermsInRules(rules).intersect(cnfNonterminals)
      if (nontermsToRemove.isEmpty)
        input
      else {
        //sort nonterminals in the increasing order of their usage
        val sortedNonterms = nontermsToRemove.toList.sortBy(nontermUses(g, _))
        removeNonterminals(g, rules, sortedNonterms)
      }
    }

    val (newg, newrules) = Util.fixpoint(removeCNFNontermsInRules)((inG, irules))
    //for each cnfnonterminal in the result, include all rules in which they occur
    val remainingRules = newg.rules.filterNot(newrules.toSet.contains)
    val leftOvers = nontermsInRules(newrules).intersect(cnfNonterminals)
    val adnlRules = if (!leftOvers.isEmpty) {
      remainingRules.filter {
        case Rule(lhs, rhs) =>
          (lhs +: rhs).exists {
            case nt: Nonterminal => leftOvers.contains(nt)
            case _ => false
          }
      }
    } else List()

    val rulesWOTemps = if (adnlRules.isEmpty)
      newrules.distinct
    else {
      //recursively remove cnfNonterminals from the rules       
      removeCNFNonterminals(newg, newrules ++ adnlRules)
    }
    //pull rules of the start symbol (if any) up
    val (firstPart, nextPart) = rulesWOTemps.partition(_.leftSide == inG.start)
    firstPart ++ nextPart
  }

 
  def cnfToGrammar[T](cnfG: Grammar[T]): Grammar[T] = {
    val cnfNonterminals = cnfG.nonTerminals.filter(isCNFNonterminal)
    //val sortedNonterms = (cnfNonterminals ++ unitNTs(cnfG)).distinct.sortBy(nontermUses(cnfG, _))
    val sortedNonterms = cnfNonterminals.distinct.sortBy(nontermUses(cnfG, _))
    val (newg, _) = removeNonterminals(cnfG, List(), sortedNonterms)
    pullStartToTop(newg)
  }

  
  /*def cyclicNonterminals(g: Grammar[T]) : Set[Nonterminal] = {   
    g.nonTerminals.toSet.filter(nt => reach(g, nt, nt))
  }  

  def cnfToGrammar(cnfG: Grammar[T]): Grammar[T] = {
    
    def selfRecursive(nt: Nonterminal, g: Grammar[T]): Boolean = {
      g.nontermToRules(nt).exists(_.rightSide.contains(nt))        
    }
    
    def isReplaceable(nt: Nonterminal, g: Grammar[T]) = {
      cnfNonterminals.contains(nt) && !selfRecursive(nt, g)
    }    
    //Inline all rules of the form N -> \alpha where N is a nonterminal created during conversion
    //optimal order of inlining is reverse topological order. But for simplicity we dont use that in the 
    //following code
    val newg = Util.fixpoint((g: Grammar[T]) => {      
      //TODO: what if one rule for a nonterminal satisfies the property while the other doesn't ??
      val nontermsToInline = g.rules.collect {
        case rule@Rule(leftSide, rightSide) if isReplaceable(leftSide, g) &&
          !rightSide.exists { case nt: Nonterminal => isReplaceable(nt, g) case _ => false } => leftSide            
      }.toSet
      val inlinedG = inlineNonterms(nontermsToInline, g)
      //remove unreachable rules from 'inlinedG'
      removeUnreachableRules(inlinedG)
    })(cnfG)   
    
    pullStartToTop(newg)
  }
  
  def removeCNFNonterminals-old(cnfG: Grammar[T], rules: List[Rule]) : List[Rule] = {
    //Note: we cannot eliminate self-recursive non-terminals
    //so they are preserved in the rules
    def getSelfrecNonterms(rules: List[Rule]): Set[Nonterminal] = {
      rules.collect {
        case Rule(lhs, rhs) if (rhs.contains(lhs)) => lhs
      }.toSet
    }
    
    val unitNTs = cnfG.nontermToRules.collect{
      case (nt, List(Rule(_, rside))) if nt != cnfG.start && !rside.contains(nt) => nt        
    }.toSet
    val selfrecNonterms = getSelfrecNonterms(cnfG.rules) 
    //val cyclicNonterms = cyclicNonterminals(cnfG)
    println("cnf Nonterms: "+cnfNonterminals)
    println("cyclicNonterms: "+cyclicNonterms)
    def irreplaceableNonterms(rules: List[Rule]) = {
      (nonterminals(cnfG) -- (cnfNonterminals ++ unitNTs)) ++ 
      	selfrecNonterms ++ getSelfrecNonterms(rules)      
    }

    //Inline all nonterminals 'N' created during CNF conversion for which there exists a 
    //production of the form N -> \alpha  in cnfG            
    val newRules = Util.fixpoint((rules: List[Rule]) => {
      
      val irreplaceables = irreplaceableNonterms(rules)      
      val nextRules = rules.flatMap {
        //rightside contains a CNF nonterminal (higher priority) ?        
        case rule @ Rule(leftSide, rightSide) if rightSide.exists {
          case nt: Nonterminal => !irreplaceables.contains(nt)
          case _ => false
        } => {
          val nonterms = rightSide.collect {
            case nt: Nonterminal if !irreplaceables.contains(nt) => nt
          }
          val newRightSides = inlineNontermsInSententialForm(nonterms.toSet, rightSide, cnfG)
          newRightSides.map { case nrside => Rule(leftSide, nrside) }
        }

        case rule @ Rule(l, r) if !irreplaceables.contains(l) => {
          val relRules = cnfG.rules.filter(_.rightSide.contains(l))          
          relRules.map {
            case Rule(rl, rr) =>
              val newrr = rr.flatMap {
                case nt: Nonterminal if nt == l => r
                case sym => List(sym)
              }
              Rule(rl, newrr)
          }
        }
        case rule@_ => List(rule)
      }
      nextRules.distinct
    })(rules)    
    
    //special handling for self-recursive nonterminals that are cnf-temporaries
    val selfrecCNFNonterms = getSelfrecNonterms(rules).intersect(cnfNonterminals)
    
    newRules
  }*/
}