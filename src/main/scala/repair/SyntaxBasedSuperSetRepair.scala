/**
 * File:    GrammarComparator.scala
 * Date:    20/5/2013
 * Author:  Mikaël Mayer
 * Purpose: Compares two grammars
 *//*
package repair
import grammar._
import CFGrammar._
import EBNFGrammar._
import generators._
import parsing._
import equivalence._
import utils._
import scala.collection.mutable.ListBuffer

*//**
 * Takes a reference grammar in any form
 *//*
class SyntaxBasedSuperSetRepair(cnfRef: Grammar) {
  import RepairResult._

  val debugSupersetIteration = true

  type GRefPair = (Rule, Rule) //a pair of G or Ref rule
  type Value = (Nonterminal, GRefPair)
  type Values = scala.collection.mutable.ListBuffer[Value]
  type NontermMap = Util.OrderedMultiMap[Nonterminal, Value]

  def gToRef(m: NontermMap) = {
    m.map(entry => (entry._1, entry._2.map(_._1).toList.mkString(" ")))
  }

  def hasManySymbols(vlist: Values) = {
    vlist.map(_._1).distinct.size >= 2
  }

  def nofOneToManyEntries(map: NontermMap) = {
    map.count { case (_, vlist) => hasManySymbols(vlist) }
  }

  *//**
   * Returns a list of feedbacks and also a mapping from newly created nonterminals to reference nonterminals
   *//*
  def makeParseTreeInfeasible(cnfG: Grammar, inputMap: Map[Nonterminal, Nonterminal], gtree: ParseTree): (List[Rule], List[Rule], Map[Nonterminal, Nonterminal]) = {

    val unmatchedNodes = findUnmatchedNodesAndNonterminalMappings(cnfG, inputMap, gtree)
    //println("Found erroneous mappings: "+unmatchedRules.flatMap(_._2).map(gToRef))

    //categorize the nodes into those for which there exists an injective mapping
    //and those for which there exists no injective mapping    
    val (rulesToRemove, mapsToSplit) = unmatchedNodes.foldLeft((Set[Rule](), Set[NontermMap]())) {
      case ((injectives, surjectives), (node, notermMap)) =>
        val nodeRule = node match {
          case Leaf.CNF(rule, _) => rule
          case Node.CNF(rule, _, _) => rule
        }
        //println("nonterm maps: "+notermMaps.map(gToRef))        
        //println("minMap: "+gToRef(minMap))        
        if (nofOneToManyEntries(notermMap) == 0) {
          (injectives + nodeRule, surjectives)
        } else {
          (injectives, surjectives + notermMap)
        }
    }
    //println("Found surjective mappings: "+mapsToSplit.map(gToRef))

    var splitMap = Map[Nonterminal, Set[Nonterminal]]()
    var rulesToAdd = Set[Rule]()
    var rulesReplaced = Set[Rule]()
    var newSymToRef = Map[Nonterminal, Nonterminal]()

    mapsToSplit.foreach(_.foreach {
      //every 'gsym' is processed only once
      case (gsym, vlist) if !splitMap.contains(gsym) && hasManySymbols(vlist) =>
        val refToNewSym = vlist.distinct.map {
          case (refSym, _) =>
            val newnt = copy(refSym)
            newSymToRef += (newnt -> refSym)
            (refSym -> newnt)
        }.toMap

        rulesReplaced ++= vlist.map(_._2._1)
        rulesToAdd ++= vlist.toList.map {
          case (refSym, (grule @ Rule(_ this should be 'gsym' , gRight),
            refrule @ Rule(_ this should be 'refSym' , refRight))) =>
            //TODO: can 'refToG' map one symbol to many ??
            val refToG = (refRight zip gRight).toMap
            val newright = refRight.mapConserve {
              case t: Terminal => t
              case nt: Nonterminal if refToNewSym.contains(nt) => refToNewSym(nt)
              case nt => refToG(nt)
            }
            val newleft = refToNewSym(refSym)
            Rule(newleft, newright)
        }
        splitMap += (gsym -> refToNewSym.values.toSet)

      case _ => ;
    })
    //Compute the relevant set of rules
    val relevantRules = ((cnfG.rules.toSet -- (rulesToRemove ++ rulesReplaced)) ++ rulesToAdd)
    var splittedRules = Set[Rule]()
    val blownupRules = splitMap.foldLeft(Set[Rule]())((acc, entry) => entry match {
      case (gsym, newsyms) =>
        val gsymRules = relevantRules.filter {
          case Rule(lhs, rhs) if (lhs == gsym || rhs.contains(gsym)) => true
          case _ => false
        }
        splittedRules ++= gsymRules
        acc ++ splitNonterminal(gsym, newsyms, gsymRules)
    })

    val newGrammarRules = (relevantRules.toSet -- splittedRules) ++ blownupRules
    val rulesChanged = cnfG.rules.toSet.intersect(splittedRules ++ rulesReplaced)    
    val rulesAdded = newGrammarRules -- cnfG.rules
    val combinedMap = inputMap ++ newSymToRef.filter(entry => !inputMap.contains(entry._1))
    (newGrammarRules.toList, rulesToRemove.toList, combinedMap)
  }

  *//**
   * Splits a nonterminal into several nonterminals
   *//*
  def splitNonterminal(oldSym: Nonterminal, newSyms: Set[Nonterminal], rules: Set[Rule]): Set[Rule] = {
    //first blow up all rights that contains 'oldSym'
    val newrules = Util.fixpoint((rls: Set[Rule]) => rls.flatMap[Rule, Set[Rule]] {
      case Rule(lside, rside) if rside.contains(oldSym) =>
        val firstIndex = rside.indexOf(oldSym)
        val prefix = rside.take(firstIndex) //this excludes 'oldSym'
        val suffix = rside.drop(firstIndex + 1) //this includes 'oldSym'
        newSyms.map(sym => (prefix :+ sym) ++ suffix).map(newRight => Rule(lside, newRight))
      case other @ _ => Set(other)
    })(rules)

    //Now, blow up all lefts containing 'oldSym'
    newrules.flatMap[Rule, Set[Rule]] {
      case Rule(lside, rside) if lside == oldSym =>
        newSyms.map(sym => Rule(sym, rside))
      case other @ _ => Set(other)
    }
  }

  *//**
   * check if every key in map1 is mapped to the same value in map2 when present in map2.
   *//*
  def consistent(map1: Map[Nonterminal, Nonterminal], map2: Map[Nonterminal, Nonterminal]): Boolean = {
    map1.forall {
      case (k, v) if map2.contains(k) => v == map2(k)
      case _ => true
    }
  }

  *//**
   * try to come up with a mapping from the nonterminals of 'cnfG' to those of 'cnfRef'
   * at every node of the parse tree. Note that the mapping can be one-to-many and many-to-one.
   * Returns a map from the nodes of the parse tree for which no cnfRef rule existed, to the best possible nonterminal mapping
   * that might be inferred for that node.
   *//*
  def findUnmatchedNodesAndNonterminalMappings(cnfG: Grammar, inputMap: Map[Nonterminal, Nonterminal], gtree: ParseTree): Map[Node, NontermMap] = {

    //We track the nodes of parse tree that does not have a counter-part in the parse trees of 'cnfRef'.
    //With each node we also record the set of mappings from the nonterminals of 'cnfG' to 'cnfRef' that
    //was discovered while traversing the parse tree until we hit the rule.
    var unmatchedNodes = Map[Node, NontermMap]()
    def recBottomUpCompare(t: ParseTree): (Set[Rule], Set[NontermMap]) = t match {
      case n @ Node.CNF(nodeRule @ Rule(lsym, List(rsym1: Nonterminal, rsym2: Nonterminal)), n1, n2) =>

        val (leftPossibs, leftMaps) = recBottomUpCompare(n1)
        val (rightPossibs, rightMaps) = recBottomUpCompare(n2)
        //println("Processing Rule: "+nodeRule)

        if (!leftPossibs.isEmpty && !rightPossibs.isEmpty) {
          val possibles = for (
            rule1 <- leftPossibs; rule2 <- rightPossibs; nt1 = rule1.leftSide; nt2 = rule2.leftSide;
            rule <- cnfRef.rules if (rule match { case r @ Rule(nt, List(`nt1`, `nt2`)) => true case _ => false })
          ) yield rule
          //consider only those possibs that are consistent with the input map
          val consistentPossibs = possibles.filter(p => consistent(Map(lsym -> p.leftSide), inputMap))
          //note: if n is the root then even if possibs is not empty it cannot have a rule 
          //that has cnfRef.start as the left side as cnfRef does not parse the string          
          val newmaps = if (!consistentPossibs.isEmpty && n != gtree) {
            consistentPossibs.flatMap {
              case refRule @ Rule(l, List(r1: Nonterminal, r2: Nonterminal)) =>
                //here we use the fact that of any symbol 'sym', the last nonterminal in the nonterm mapping of 'sym' 
                //is the most recently added nonterminal                
                val lmaps = leftMaps.filter(m => m(rsym1).last._1 == r1)
                val rmaps = rightMaps.filter(m => m(rsym2).last._1 == r2)
                //combine the left, right mappings and (lsym -> l) to create a new mapping
                for (lmap <- lmaps; rmap <- rmaps) yield {
                  lmap.append(rmap).addBinding(lsym, (l, (nodeRule, refRule)))
                }
            }
          } else Set[NontermMap]()

          if (!newmaps.isEmpty) {
            //println("Found possible rules: "+possibs)            
            //println("Found possible mappings: "+newmaps.map(gToRef))
            (consistentPossibs, newmaps)
          } else {
            //here, the reference grammar did not find any rules to concatenate the left and right subtrees
            val combinedMaps = for (lmap <- leftMaps; rmap <- rightMaps) yield lmap.append(rmap)
            //check if there is at least one homomorphism from cnfG to cnfRef
            val minMap = combinedMaps.min(Ordering.by[NontermMap, Int](nofOneToManyEntries))

            //for debugging
            if (this.debugSupersetIteration) {
              if (!possibles.isEmpty && consistentPossibs.isEmpty) {
                println("Conflit at Rule: " + nodeRule + " with " + inputMap)
              } else {
                println("No corresponding rule in reference for node: " + nodeRule)
              }
            }

            unmatchedNodes += (n -> minMap)
            //println("Found erroneous mappings: "+erroneousmaps.map(gToRef))
            //no rules are possible
            (Set(), Set())
          }
        } else (Set(), Set())
      case l @ Leaf.CNF(leafRule @ Rule(lsym, _), t) =>
        //println("Processing Rule: "+leafRule)
        // Find all rules in cnfG that can produce terminal t, if none exists create a new one
        val possibles = cnfRef.rules.collect { case rule @ Rule(nt, List(`t`)) => rule }.toSet
        val consistentPossibs = possibles.filter(p => consistent(Map(lsym -> p.leftSide), inputMap))
        val newmaps = if (!consistentPossibs.isEmpty && l != gtree) {
          //here, create new mappings from nonterminals of 'cnfG' to 'cnfRef'
          //make sure that the new mappings are consistent with the 'inputMap'
          consistentPossibs.collect {
            case refRule @ Rule(l, _) =>
              val newmap = new NontermMap
              newmap.addBinding(lsym, (l, (leafRule, refRule)))
          }
        } else Set[NontermMap]()

        if (!newmaps.isEmpty) {
          (consistentPossibs, newmaps)
        } else {
          //for debugging
          if (this.debugSupersetIteration) {
            if (!possibles.isEmpty && consistentPossibs.isEmpty) {
              println("Conflit at Rule: " + leafRule + " with: " + inputMap)
            } else {
              println("No corresponding rule in the reference for node: " + leafRule)
            }
          }
          unmatchedNodes += (l -> new NontermMap())
          (Set(), Set())
        }
    }

    recBottomUpCompare(gtree)
    if (unmatchedNodes.isEmpty)
      throw new IllegalStateException("No unmatched node in the parse tree !!")
    unmatchedNodes //map {  case (r, nontermMap) => (r, mset.map(_.distinct)) }
  }
}*/