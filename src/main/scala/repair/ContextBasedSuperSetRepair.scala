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
import RepairResult._

class ContextBasedSuperSetRepair[T](g: Grammar[T], ungenWord: Word[T], equivChecker: EquivalenceChecker[T])
	(implicit gctx: GlobalContext, opctx: RepairContext) {

  sealed abstract class Permissibility
  case class PermissibleUnderContext() extends Permissibility
  case class PermissibleInOtherContexts() extends Permissibility
  case class NotPermissible() extends Permissibility

  val cnfG = g.cnfGrammar 
  val gParser = new CYKParser[T](cnfG)
  val nontermProductions = g.nontermToRules.map { case (k, v) => (k, v.map(_.rightSide).toSet) }
  val refWords = equivChecker.getRefWords 

  def findNonterminalWithProductions(prods: Set[List[Symbol[T]]]): Option[Nonterminal] = {
    nontermProductions.collectFirst {
      case (nt, ntRights) if (ntRights == prods) => nt
    }
  }

  lazy val (permissibleParseTrees, acceptedWords) = {    
    //get the parse trees of cnfG of all the valid strings of the reference grammar    
    refWords.foldLeft((List[ParseTree[T]](), List[Word[T]]())) {
      //check for abort flag
      case (acc, _) if gctx.abort => acc
      case ((accTrees, accWords), refWord) if accWords.size < opctx.nCorrectWordsForRepair => {
        gParser.parseWithTree(refWord) match {
          case Some(tree) =>
            //for debugging
            /*val intTerm = Terminal[T]("Int")          
          if (refWord == List(intTerm, Terminal[T](","), intTerm, Terminal[T]("=>"), intTerm, Terminal[T]("$"))) {
            println("ParseTree[T] for "+refWord+": \n"+tree)
            oneTree = tree
            println(wordToString(refWord))
          }*/            
            (accTrees :+ tree, accWords :+ refWord)
          case None =>
            (accTrees, accWords)
        }
      }
      case (acc, _) => acc
    }
  }

  var childrenCache = Map[Rule[T], Set[(Rule[T], Int)]]()
  /**
   * Collects the set of rules of the nodes that are children
   * of nodes containing context.
   */
  def children(context: Rule[T]): Set[(Rule[T], Int)] = {

    def collectChildren(tree: ParseTree[T]): Set[(Rule[T], Int)] = {
      tree match {
        case Node(_, List()) | Leaf(_) => Set()
        case Node(rule, children) =>
          val foundChildren = (children flatMap {
            case cn: Node[T] => collectChildren(cn)
            case _ => List()
          }).toSet
          if (rule == context) {
            foundChildren ++ {
              children.zipWithIndex.collect {
                case (Node(r, _), i) => (r, i)
              }.toSet
            }
          } else {
            foundChildren
          }
      }
    }

    val children = childrenCache.getOrElse(context, {
      val chs = permissibleParseTrees.foldLeft(Set[(Rule[T], Int)]())((acc, ptree) => acc ++ collectChildren(ptree))
      childrenCache += (context -> chs)
      chs
    })
    children
  }

  /**
   * Finds all the rules of the parent nodes of 'key' in the given 'tree'.
   * A parent is a rule and also the index of the rightSIde at which the 'tree' appears
   * Assumes that the root has already been processed.
   * This relies on the fact that the start symbol does not appear on the left side of productions
   * TODO: This code could be really slow O(n^2) where 'n' is the size of the tree. Find a faster algorithm.
   */
  def collectParents(tree: ParseTree[T], key: ParseTree[T]): Set[(Rule[T], Int)] = {
    tree match {
      case Node(_, List()) | Leaf(_) => Set()
      case Node(nr, children) =>
        val foundParents = (children flatMap {
          case cn: Node[T] => collectParents(cn, key)
          case _ => List()
        }).toSet
        foundParents ++ children.zipWithIndex.collect {
          case (`key`, i) => (nr, i)
        }.toSet
    }
  }

  /**
   * TODO: I am sure there is a better way to identify this using just the
   * rules and the ref words
   */
  def isTreePermissible(subtree: ParseTree[T], context: Rule[T], index: Int): Permissibility = {
    val res = permissibleParseTrees.foldLeft(NotPermissible(): Permissibility)((acc, ptree) => acc match {
      case PermissibleUnderContext() => {
        acc
      }
      case _ =>
        val parents = collectParents(ptree, subtree)
        if (!parents.isEmpty) {
          if (parents.contains((context, index))) {
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

  sealed abstract class RepairType
  object Aborted extends RepairType
  case class RemoveRule(rule: Rule[T]) extends RepairType
  case class ExpandRightSides(rule: Rule[T]) extends RepairType  
  case class PreventUnderContext(rule: Rule[T], index: Int, context: Rule[T]) extends RepairType

  /**
   * This procedure identifies the rules and the repairs that have to be performed
   */
  @tailrec final def findRepairPoint(tree: ParseTree[T]): RepairType = tree match {
    case _ if gctx.abort =>
      Aborted
    case n @ Node(contextRule, children) =>
      val repairPoint = children.zipWithIndex.foldLeft(None: Option[(Permissibility, ParseTree[T], Int)]) {
        case (None, (childTree, index)) =>
          isTreePermissible(childTree, contextRule, index) match {
            case PermissibleUnderContext() =>
              //in this case a fix cannot be found inside the 'childTree' so move to the next child
              None
            case res @ _ =>
              Some((res, childTree, index))
          }
        case (acc, _) => acc
      }

      if (opctx.debugSupersetRepair)
        println("repairPoint: " + repairPoint)
      repairPoint match {
        case None =>
          //Here, every children is individually feasible but the combination of child trees is not feasible.
          //To identify the repair point, unfold the current rule so that every possible 
          //combination of the productions of the nonterminals on the right hand side are made explicit. 
          //Hence, in the subsequent iterations the current invalid combinations can be eliminated by splitting
          ExpandRightSides(contextRule)
        case Some((_, childTree: Leaf[T], _)) =>
          //here the terminal is not feasible under the given context.
          //Therefore, the only fix is to remove the context rule
          RemoveRule(contextRule)
        case Some((PermissibleInOtherContexts(), Node(r, _), index)) =>
          //the fix lies in this child
          PreventUnderContext(r, index, contextRule)
        case Some((NotPermissible(), child: Node[T], _)) =>
          //recurse into the child tree as we have found a smaller infeasible subtree
          findRepairPoint(child)
        case Some(_) =>
          throw new IllegalStateException("Impossible match case taken !!")
      }
    case l: Leaf[T] =>
      //we should be hitting this case 
      throw new IllegalStateException("The parse tree starts with a leaf: " + l)
  }

  /**
   * Returns the actual fix performed and the result of the repair as
   * a mapping from the old rules to the new rules that replaces it.
   */  
  def eliminateAParseTree(): GrammarFeedback[T] = {

    val gtree = gParser.parseWithTree(ungenWord).get
    if (opctx.debugSupersetRepair)
      println("Input parse tree: " + gtree)

    //when there is no permissible parse trees the grammar doesn't accept any valid string
    //hence, remove the first production      
    if (permissibleParseTrees.isEmpty && !gctx.abort) {
      RemoveRules(List(gtree.asInstanceOf[Node[T]].r))
    } else if (gctx.abort) {
      NoRepair("Operation Aborted.")
    } else {
      val repairType = findRepairPoint(gtree)
      if (opctx.debugSupersetRepair) {
        //println("context rule: "+context+" repair point: "+repairPoint)
        println("Repair Type: " + repairType.toString)
      }
      repairType match {
        case _ if gctx.abort =>
          NoRepair("Operation Aborted.")
        case RemoveRule(rule) =>
          RemoveRules(List(rule))
        case ExpandRightSides(rule @ Rule(lhs, rhs)) =>
          //here, inline the right sides
          val nontermsToInline = rhs.collect { case nt: Nonterminal => nt }.toSet
          val newRightSides = inlineNontermsInSententialForm(nontermsToInline, rhs, g)
          val newRules = newRightSides.map(rside => Rule(lhs, rside))
          ExpandRules(List(rule), newRules)        

        case PreventUnderContext(repairRule, index, context) =>
          //Fix(a) check if the 'contextRule' is actually a redundant rule        
          //TODO: how can we abort the operation here ?
          if(isRedundantRule(repairRule)){
             //'repairRule' is redundant                       
             RemoveRules(List(repairRule), None)
          }
          else if(isRedundantRule(context)) {
             //'contextRule' is redundant                      
             RemoveRules(List(context), Some(List(repairRule)))
          }  
          else {
            //Fix(b): we need to  make the 'repairRule' impossible in context
            //without affecting the set of strings that are accepted 
            val (splitNT, newRules) = findReplacement(repairRule, index, context)
            //replace the 'repairRule.leftSide' in the contextRule with 'splitNT'
            val newContextRule = Rule(context.leftSide, context.rightSide.zipWithIndex.map {
              case (sym, `index`) => splitNT
              case (sym, i) => sym
            })

            if (opctx.debugSupersetRepair)
              println("Replacing: " + context + " by \n" + (newContextRule +: newRules.toList).mkString("\n"))
            //Map(context -> (newContextRule +: newRules.toList))
            RefineRules(List(context), (newContextRule +: newRules.toList), List(repairRule))
          }
      }
    }
  }    

  def isRedundantRule(rule: Rule[T]) = {
    val newCnf = Grammar[T](g.start, g.rules.filterNot(_ == rule)).cnfGrammar    
    if (!newCnf.rules.isEmpty) {
      val cykparser = new CYKParser(newCnf)
      val ctex = acceptedWords.find(x => !cykparser.parse(x))
      if (!ctex.isDefined) {
        //every word that was previously accepted are also accepted now
        true
      } else false
    } else false
  }  

  /**
   * Find a replacement for the nonterminal 'refineNT' for use in the  given context
   * that does not have refineProd, we need to have 'refineNT' as it is for use in other contexts.
   * As an optimization check what are the rules of the left side of 'repairRule'
   * that are actually used by the context and add only those to the new nonterminal
   */
  def findReplacement(refineRule: Rule[T], index: Int, context: Rule[T]): (Nonterminal, Seq[Rule[T]]) = {
    //val splitProds = nontermProductions(repairRule.leftSide) -- Set(repairRule.rightSide)
    val usedProds = children(context).collect {
      case (r, `index`) if r.leftSide == refineRule.leftSide => r
    }.map(_.rightSide)
    val splitProds = usedProds
      /*//here, check if
      if (isRulePermissible(r, contextRule, index)) {      
      usedProds -- Set(refineRule.rightSide)*/

    //check if the 'nontermProductions' map already contains a non-terminal for this split 
    val existingNT = findNonterminalWithProductions(splitProds)    
    if (existingNT.isDefined && existingNT.get != refineRule.leftSide) //note: we do not want to add the same rule again
      (existingNT.get, Seq())
    else {
      var foundReplacement: Option[Nonterminal] = None
      if (opctx.enableExpensiveRepair) {
        //check here if any other non-terminal in the grammar can be used in place of refineNT
        //note that such a non-terminal should have the same form as spiltProds i.e,
        // it cannot have any more or any less productions, it should also agree on all terminals
        //TODO: this has connections to minimizing a grammar
        val candidates = g.nontermToRules.foldLeft(List[Nonterminal]()) {
          case (acc, (nt, rules)) =>
            if (rules.size == splitProds.size) {
              if (splitProds.exists { sp =>
                val foundMatch = rules.exists(rl => (sp zip rl.rightSide).forall {
                  case (t1: Terminal[T], t2: Terminal[T]) if (t1 == t2) => true
                  case (nt1: Nonterminal, nt2: Nonterminal) => true
                  case _ => false
                })
                !foundMatch
              }) {
                //here not every rule has a match, so skip this non-terminal
                acc
              } else {
                //here every rule has match (though it may not be one-to-one which is not checked)
                acc :+ nt
              }
            } else acc //here there cannot be a one-to-one match for every rule            
        }
        if (opctx.debugSupersetRepair) {
          println("Candidates chosen: " + candidates.mkString(","))
        }
        //try each candidate to replace the non-terminal at the 'index' of the right-side of the
        //context rule
        val ruleWOCtx = g.rules.filterNot(_ == context)
        foundReplacement = candidates.find { cand =>
          val newContextRule = Rule(context.leftSide, context.rightSide.zipWithIndex.map {
            case (sym, `index`) => cand
            case (sym, _) => sym
          })
          val candidateCNF = Grammar[T](g.start, ruleWOCtx :+ newContextRule).cnfGrammar 
          val candParser = new CYKParser(candidateCNF)
          //check if candidateCNF does not accept the ungeneratable word
          //Here, we are using CYK parser, hence, this could be expensive
          if (!candParser.parse(ungenWord)) {
            //check if the acceptance of the grammar is preserved
            val ctex = acceptedWords.find(x => !candParser.parse(x)) 
            if (!ctex.isDefined) {
              //TODO: is this necessary ?
              //check if the new grammar is a subset of the old
              /* val ctex2 = (new EquivalenceChecker(g)).counterExampleForInclusion(candidateCNF)
              if (ctex2.isEmpty) {*/
              if (opctx.debugSupersetRepair) {
                println("Found a replacement: " + cand)
              }
              true
              //} else false
            } else false
          } else false
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

  /**
   * Splits a nonterminal into several nonterminals
   */
  def splitNonterminal(oldSym: Nonterminal, newSyms: Set[Nonterminal], rules: Set[Rule[T]]): Set[Rule[T]] = {
    //first blow up all rights that contains 'oldSym'
    val newrules = Util.fixpoint((rls: Set[Rule[T]]) => rls.flatMap[Rule[T], Set[Rule[T]]] {
      case Rule(lside, rside) if rside.contains(oldSym) =>
        val firstIndex = rside.indexOf(oldSym)
        val prefix = rside.take(firstIndex) //this excludes 'oldSym'
        val suffix = rside.drop(firstIndex + 1) //this includes 'oldSym'
        newSyms.map(sym => (prefix :+ sym) ++ suffix).map(newRight => Rule(lside, newRight))
      case other @ _ => Set(other)
    })(rules)

    //Now, blow up all lefts containing 'oldSym'
    newrules.flatMap[Rule[T], Set[Rule[T]]] {
      case Rule(lside, rside) if lside == oldSym =>
        newSyms.map(sym => Rule(sym, rside))
      case other @ _ => Set(other)
    }
  }
}