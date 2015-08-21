/**
 * File:    GrammarComparator.scala
 * Date:    20/5/2013
 * Author:  Mikaël Mayer
 * Purpose: Compares two grammars
 */
package repair
import grammar._
import CFGrammar._
import EBNFGrammar._
import generators._
import parsing._
import equivalence._
import utils._
import scala.collection.mutable.ListBuffer
import scala.annotation.tailrec

/**
 * Takes a reference grammar in any form
 */
class SubsetRepair(equivChecker: EquivalenceChecker)
	(implicit gctx: GlobalContext, opctx: RepairContext) {
  import RepairResult._  

  val cnfRef = equivChecker.refg.cnfGrammar 

  def makeParsable(g: Grammar, unparsableWord: List[Terminal]) = {

    if (g.rules.isEmpty) {
      throw new IllegalStateException("No rules found in the input grammar to repair!!")
    } else {
      //val refTree = CYKParser.parseWithTree(cnfRef, unparsableWord).get
      val refTree = equivChecker.refParser.parseWithTree(unparsableWord).get
      if (opctx.debugSubsetRepair)
        println("Ref parse tree: " + refTree)      

      //println("Ref parse tree: "+refTree)
      var rulesHypothesized = Set[Rule]()
      // Each node stores possibles rules of the solution which could apply
      var possibleRules = Map[Node, Set[Rule]]()
      val nontermMapping = new Util.MultiMap[Nonterminal, Nonterminal]()

      //StartRef matches startG and vice-versa. 
      //We make sure that startG  matches nothing else
      nontermMapping.addBinding(cnfRef.start, g.start)

      def recBottomUpCompare(subtree: ParseTree): Unit = subtree match {
        case pnode @ Node(nr @ Rule(ntLeft, _), children) =>
          //invoke the procedure recursively on each of the children
          children.map(recBottomUpCompare)
          val childSymsList = children.map {
            case Leaf(t) => Set(t.asInstanceOf[Symbol])
            case chnode: Node => possibleRules(chnode).map(_.leftSide.asInstanceOf[Symbol])
          }
          //construct the set of possible rules of 'g' that matches childPossibs
          val initPots = g.rules.filter(_.rightSide.size == childSymsList.size)
          val (matchingRules, _) = childSymsList.foldLeft((initPots, 0)) {
            case ((potPossibs, index), childSyms) =>
              //pick all rules of potPossibs whose 'index'th symbol of the rightside 
              //matches the leftside of a rule in childRules              
              val newPots = potPossibs.filter {
                case r @ Rule(_, rside) if (rside.size > index &&
                  childSyms.contains(rside(index))) =>
                  true
                case _ =>
                  false
              }
              (newPots, index + 1)
          }

          //we need to handle 'start' node case specially
          val (startRules, nonstartRules) = matchingRules.partition(_.leftSide == g.start)
          val possibs = if (ntLeft == cnfRef.start)
            startRules
          else
            nonstartRules

          val rulesForNode = if (!possibs.isEmpty) {
            possibs.toSet
          } else {
            val emptySententialForm = List[Symbol]()
            val newRsides = childSymsList.foldLeft(List(emptySententialForm)) {
              case (partialRsides, childSyms) =>
                //take cartesian product of partialRsides and childLefts
                for (partR <- partialRsides; nextNT <- childSyms)
                  yield partR :+ nextNT
            }
            val lsides = nontermMapping.getOrElse(ntLeft, 
                Set(Nonterminal(Util.freshName())) //create a fresh nonterminal for left side
                ).toList
            val newrules = newRsides.flatMap(rside => lsides.map(Rule(_, rside)))
            rulesHypothesized ++= newrules
            //println("Newrules: "+newrules)
            newrules.toSet
          }
          if (opctx.debugSubsetRepair)
            println("Rules for " + nr + " : " + rulesForNode)
          //update state
          rulesForNode.foreach(rule => nontermMapping.addBinding(ntLeft, rule.leftSide))
          possibleRules += (pnode -> rulesForNode)

        case Leaf(t) =>
          //nothing has to be done here.
          ;
      }

      var chosenRules = Map[Node,Rule]()
      def recTopDownGenerate(subtree: ParseTree, nt: Nonterminal) :  Unit = subtree match {
        case pnode @ Node(_, children) => {
          //pick a possible rule with 'nt' on the leftSide that has the minimum size for the right side          
          val minRule = possibleRules(pnode).filter(_.leftSide == nt).min(Ordering.by[Rule, Int] {
            //preferring rules with just terminals more
            case Rule(_, rside) if rside.forall(_.isInstanceOf[Terminal]) => 0
            case Rule(_, rightSide) => rightSide.size
          })
          chosenRules += (pnode -> minRule)
          minRule match {
            case Rule(_, rside) if rside.forall(_.isInstanceOf[Terminal]) =>
              ; //need not do any thing
            case Rule(_, rside) =>
              (children zip rside).foreach {
                case (childNode, childNT: Nonterminal) =>
                  recTopDownGenerate(childNode, childNT)
                case _ => 
                  ;
              }
          }
        }
        case Leaf(t) =>
          ; //this case is not possible so return empty list of rules          
      }

      recBottomUpCompare(refTree)
      //println("Possible Rules: "+possibleRules)
      if (rulesHypothesized.isEmpty)
        throw new IllegalStateException("No rules added by repair !!")
      recTopDownGenerate(refTree, g.start)
      val fixes = chosenRules.values.filter(rulesHypothesized.contains).toList.distinct
      //find every known non-terminal in the right 
      RepairResult.AddAllRules(fixes)
    }
  }
}