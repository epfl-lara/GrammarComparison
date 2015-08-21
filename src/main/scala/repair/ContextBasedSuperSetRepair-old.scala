/*package repair
import grammar._
import CFGrammar._
import EBNFGrammar._
import generators._
import parsing._
import equivalence._
import utils._
import scala.collection.mutable.ListBuffer
import scala.annotation.tailrec


class ContextBasedSuperSetRepair(cnfG: Grammar, equivChecker: EquivalenceChecker)
	(implicit opctx: OperationContext) {

  sealed abstract class Permissibility
  case class PermissibleUnderContext() extends Permissibility
  case class PermissibleInOtherContexts() extends Permissibility
  case class NotPermissible() extends Permissibility

  lazy val (permissibleParseTrees, acceptedWords) = {
    //get the parse trees of cnfG of all the valid strings of the reference grammar
    equivChecker.words.foldLeft((List[ParseTree](), List[Word]())) {
      //check for abort flag
      case (acc, _) if opctx.abort => acc
      case ((accTrees, accWords), refWord) if accWords.size < opctx.nCorrectWordsForRepair => {
        CYKParser.parseWithTree(cnfG, refWord) match {          
          case None =>          
          throw new IllegalStateException("The superset grammar does not parse the valid word: " + refWord + " Grammar: " + cnfG)
          case Some(tree) =>
            //for debugging
            val intTerm = Terminal("Int")          
          if (refWord == List(intTerm, Terminal(","), intTerm, Terminal("=>"), intTerm, Terminal("$"))) {
            println("ParseTree for "+refWord+": \n"+tree)
            oneTree = tree
          }
            (accTrees :+ tree, accWords :+ refWord)
          case None =>
            (accTrees, accWords)
        }
      }
      case (acc, _) => acc  
    }
  }

  *//**
   * Returns all the parents all the sub-trees of 'tree' that are equal to 'subtree'
   * Assuming that the equality of trees is deep and does not look at references.
   * This code could be really slow O(n^2) where 'n' is the size of the tree. Find a faster algorithm.
   *//*
  def getRule(t: ParseTree): Rule = t match {
    case Node.CNF(r, _, _) => r
    case Leaf.CNF(r, _) => r
  }

  var childrenCache = Map[Rule, Set[Rule]]()
  *//**
   * Collects the set of rules of the nodes that are children
   * of nodes containing context.
   *//*
  def children(context: Rule): Set[Rule] = {

    def collectChildren(tree: ParseTree): Set[Rule] = {
      tree match {
        case Node(_, List()) | Leaf.CNF(_, _) => Set()
        case n @ Node.CNF(rule, left, right) =>
          val foundChildren = collectChildren(left) ++ collectChildren(right)
          if (rule == context) {
            foundChildren + getRule(left) + getRule(right)
          } else {
            foundChildren
          }
      }
    }

    val children = childrenCache.getOrElse(context, {
      val chs = permissibleParseTrees.foldLeft(Set[Rule]())((acc, ptree) => acc ++ collectChildren(ptree))
      childrenCache += (context -> chs)
      chs
    })
    children
  }

  *//**
   * Finds all the rules of the parent nodes of 'key' in the given 'tree'
   * Assumes that the root has already been compared.
   * This relies on the fact that the start symbol does not appear on the left side of productions
   *//*
  def collectParents(tree: ParseTree, key: ParseTree): Set[Rule] = {
    //val initFound = if (subtree == tree) true else false
    tree match {
      case Node(_, List()) | Leaf.CNF(_, _) => Set()
      case n @ Node.CNF(_, left, right) =>
        val foundParents = collectParents(left, key) ++ collectParents(right, key)
        if (left == key || right == key) {
          foundParents + getRule(n)
        } else {
          foundParents
        }
    }
  }

  *//**
   * TODO: I am sure there is a better way to identify this using just the
   * rules and the ref words
   *//*
  def isTreePermissible(subtree: ParseTree, context: Rule): Permissibility = {
    val res = permissibleParseTrees.foldLeft(NotPermissible(): Permissibility)((acc, ptree) => acc match {
      case PermissibleUnderContext() => {
        acc
      }
      case _ =>
        val parents = collectParents(ptree, subtree)
        if (!parents.isEmpty) {
          if (parents.contains(context)) {
            //println("Parents for "+getRule(subtree)+": "+parents)         
            //println("Permissible under context: "+(getRule(subtree),context)+" in tree \n "+ptree)        
            PermissibleUnderContext()
          } else PermissibleInOtherContexts()
        } else
          acc
    })
    //println("Permissibility result for (subtree, context): "+(getRule(subtree),context)+" : "+res)
    res
  }

  *//**
   * Checks if the rule 'key' is permissible under the given context
   *//*
  def isRulePermissible(key: Rule, context: Rule): Boolean = {
    children(context).contains(key)
  }

  val nontermProductions = cnfG.nontermToRules.map { case (k, v) => (k, v.map(_.rightSide).toSet) }

  def findNonterminalWithProductions(prods: Set[List[Symbol]]): Option[Nonterminal] = {
    nontermProductions.collectFirst {
      case (nt, ntRights) if (ntRights == prods) => nt
    }
  }

  sealed abstract class RepairType
  object Aborted extends RepairType
  case class RemoveRule(rule: Rule) extends RepairType
  case class ExpandRightSides(rule: Rule) extends RepairType
  case class PreventUnderContext(rule: Rule, context: Rule) extends RepairType

  *//**
   * This procedure identifies the rules and the repairs that has to be performed
   *//*
  @tailrec final def findRepairPoint(tree: ParseTree): RepairType = tree match {
    case _ if opctx.abort => 
      Aborted
    case n @ Node.CNF(contextRule, leftTree, rightTree) => {
      //check if the children are subtrees of a parse tree of a valid string
      isTreePermissible(leftTree, contextRule) match {
        case PermissibleUnderContext() =>
          //in this case a fix cannot be found inside the 'leftTree' so move to the right tree
          isTreePermissible(rightTree, contextRule) match {
            case PermissibleUnderContext() =>
              //Here, both left and right sub-trees are individually feasible but
              //the combination of left and right sub-trees is not feasible.
              //To identify this, unfold the current rule so that every possible 
              //combination of the productions of the left and right nonterminals
              //are made explicit. Hence, in the subsequent iterations the current invalid combinations can
              //be eliminated by splitting
              ExpandRightSides(contextRule)

            case PermissibleInOtherContexts() =>
              //the fix lies in the righttree, however, the rule at the top may just be feasible under the context,
              //in which case we need inline the rules
              if (isRulePermissible(rightTree.r, contextRule))
                //here we have to inline
                ExpandRightSides(rightTree.r)
              else {
                //here we can make the rule of the rightTree infeasible under the given context
                PreventUnderContext(rightTree.r, contextRule)
              }
            case NotPermissible() =>
              //recurse into the rightTree as we have found a smaller infeasible context
              findRepairPoint(rightTree)
          }
        case PermissibleInOtherContexts() =>
          //the fix lies in the lefttree, however, the rule at the top may just be feasible under the context,
          //in which case we need inline the rules
          if (isRulePermissible(leftTree.r, contextRule))
            //here we have to inline
            ExpandRightSides(leftTree.r)
          else {
            //here we can make the rule of the rightTree infeasible under the given context
            PreventUnderContext(leftTree.r, contextRule)
          }
        case NotPermissible() =>
          //recurse into the leftTree as we have found a smaller context
          findRepairPoint(leftTree)
      }
    }
    case l @ Leaf.CNF(contextRule, t) => {
      //here the leaf rule itself is not valid. Therefore, the only fix is to remove the
      //leaf rule
      RemoveRule(contextRule)
    }
  }

  *//**
   * Returns the actual fix performed and the result of the repair as
   * a mapping from the old rules to the new rules that replaces it.
   *//*
  //def makeParseTreeInfeasible(gtree: ParseTree): Map[Rule, List[Rule]] = {
  import RepairResult._
  def makeParseTreeInfeasible(gtree: ParseTree): CNFFeedback = {
    if (opctx.debugSupersetRepair)
      println("Input parse tree: " + gtree)

    //when there is no permissible parse trees the grammar doesn't accept any valid string
    //hence, remove the first production      
    if (permissibleParseTrees.isEmpty && !opctx.abort) {
      CNFRemove(gtree.asInstanceOf[Node].r)
    } else if(opctx.abort) {
      NoRepair("Operation Aborted.")
    } else {
      val repairType = findRepairPoint(gtree)      
      if (opctx.debugSupersetRepair) {
        //println("context rule: "+context+" repair point: "+repairPoint)
        println("Repair Type: " + repairType.toString)
      }
      repairType match {
        case _ if opctx.abort => 
          NoRepair("Operation Aborted.")
        case RemoveRule(rule) =>
          //Map(rule -> List())
          CNFRemove(rule)
        case ExpandRightSides(rule @ Rule(lhs, rhs)) =>
          //here, inline the right sides
          val nontermsToInline = rhs.collect { case nt: Nonterminal => nt }.toSet
          val newRightSides = inlineNontermsInSententialForm(nontermsToInline, rhs, cnfG)
          val newRules = newRightSides.map(rside => Rule(lhs, rside))
          //Map(rule -> newRules)
          CNFExpand(rule, newRules)
        case PreventUnderContext(repairRule, context) =>
          //Fix(a) check if the 'contextRule' is actually a redundant rule        
          val removable = if (context.leftSide == cnfG.start) {
            cnfG.nontermToRules(cnfG.start).size > 1 //check if there exists more than one start rule if we are removing a start rule
          } else true

          //TODO: how can be abort the operation here ?
          val foundFix = if (removable) {            
            val newg = Grammar(cnfG.start, cnfG.rules.filterNot(_ == context))
            val newCnf = CNFConverter.toCNF(newg)            
            val ctex = equivChecker.findUnacceptedWord(newCnf, acceptedWords)
            if (!ctex.isDefined) {
              //every word that was previously accepted are also accepted now
              //println("No counter example found!!")
              //'contextRule' is redundant as every word is parsable even without this        
              //Map(context -> List())
              Some(CNFRemove(context, Some(repairRule)))
            } else None
          } else None

          if (foundFix.isDefined)
            foundFix.get
          else {                        
            //Fix(b): we need to  make the 'repairRule' impossible in context
            //without affecting the set of strings that are accepted 
            val (splitNT, newRules) = findReplacement(repairRule, context)
            //replace the 'repairRule.leftSide' in the contextRule with 'splitNT'
            val newContextRule = Rule(context.leftSide, context.rightSide.map {
              case sym @ _ if (sym == repairRule.leftSide) => splitNT
              case sym @ _ => sym
            })

            if (opctx.debugSupersetRepair)
              println("Replacing: " + context + " by \n" + (newContextRule +: newRules.toList).mkString("\n"))
            //Map(context -> (newContextRule +: newRules.toList))
            CNFSplit(context, (newContextRule +: newRules.toList), repairRule)
          }
      }
    }
  }

  *//**
   * Find a replacement for the nonterminal 'refineNT' for use in the  given context
   * that does not have refineProd, we need to have 'refineNT' as it is for use in other contexts. 	        	        
   * As an optimization check what are the rules of the left side of 'repairRule' 
   * that are actually used by the context and add only those to the new nonterminal
   *//*
  def findReplacement(refineRule: Rule, context: Rule): (Nonterminal, Seq[Rule]) = {    
    //val splitProds = nontermProductions(repairRule.leftSide) -- Set(repairRule.rightSide)
    val usedProds = children(context).filter(_.leftSide == refineRule.leftSide).map(_.rightSide)
    val splitProds = usedProds -- Set(refineRule.rightSide)

    //check if the 'nontermProductions' map already contains a non-terminal for this split 
    val existingNT = findNonterminalWithProductions(splitProds)
    if (existingNT.isDefined)
      (existingNT.get, Seq())
    else {
      var foundReplacement: Option[Nonterminal] = None
      if (opctx.enableExpensiveRepair) {
        //check here if any other non-terminal in the grammar can be used in place of refineNT
        //note that such a non-terminal should have the same form as spiltProds i.e,
        // it cannot have any more or any less productions
        val refineGRules = 
        cnfG.nontermToRules.foldLeft(List[Nonterminal]()){
          case (acc, (nt, rules)) =>
            //first remove cnf nonterminals from the rules
            
        }
      }
      if (foundReplacement.isEmpty) {
        //here, we need to create a new non-terminal as nothing in the grammar can be reused
        val newnt = copy(refineRule.leftSide)
        (newnt, splitProds.map(prod => Rule(newnt, prod)).toList)
      } else
        (foundReplacement.get, Seq())
    }
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
}*/