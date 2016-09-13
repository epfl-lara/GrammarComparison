package parsing

import grammar._
import utils._
import Logging._
import CFGrammar._
import CNFConverter._

/**
 * A converter to Greiback Normal Form GNF
 * Requires: grammars to be in CNF
 */
object GNFConverter {

  //var gnfNonterminals = Set[Nonterminal]()
  def freshNonterminal(name: Option[String]): Nonterminal = {
    val freshSym = Nonterminal(Util.freshName(name))
    //gnfNonterminals += freshSym
    freshSym
  }

  /**
   * This handles immediate left recursion.
   * The first component of the return value is the modified right recursive rules
   * and the second component are the Z rules
   */
  def removeLeftRecursion(ntrules: List[Rule], nt: Nonterminal): (List[Rule], List[Rule]) = {

    def isLeftRecursive(rule: Rule): Boolean = rule match {
      case Rule(lhs, head :: _) if lhs == head => true
      case _ => false
    }
    //collect all left recursive rules
    val (leftRecur, rest) = ntrules.partition(isLeftRecursive)
    if (leftRecur.isEmpty)
      (ntrules, List())
    else {
      val alphas = leftRecur.map(_.rightSide.tail)
      val betas = rest.map(_.rightSide)
      val Z = Nonterminal(Util.freshName(Some(nt.name)))
      (rest ++ betas.map(beta => Rule(nt, beta :+ Z)), 
        alphas.flatMap(alpha => List(Rule(Z, alpha :+ Z), Rule(Z, alpha))))
    }
  }

  def inlineFirstNonterminal(ruleList: List[Rule], rule: Rule): List[Rule] = rule match {
    case rule @ Rule(lhs, ((head: Nonterminal) :: tail)) =>
      val inlinedRules = ruleList.collect {
        case Rule(`head`, rightSide) => Rule(lhs, rightSide ++ tail)
      }
      inlinedRules
    case _ => List(rule)
  }    

  def toGNF[T](ing: Grammar[T])(implicit opctx: GlobalContext): Grammar[T] = {
    val g = ing.cnfGrammar //first convert the grammar to CNF 

    val dumpGrammar = (title: String) => (g: Grammar[T]) => {
      println(title + " phase: ");
      println("----------------");
      println(g);
      println("=======");
      g
    }
    //dumpGrammar("Initial grammar")(g)

    //order the nonterminals from A1 ... An, where A1 is the start.    
    val grules = g.rules.filterNot(_.rightSide.isEmpty)
    val nonterms = g.start +: g.rules.map(_.leftSide).filterNot(_ == g.start).distinct
    val ntIndex = nonterms.zipWithIndex.toMap    

    //convert every rule A_i -> A_j \alpha such that j >= i
    def orderRule(ruleList: List[Rule], rule: Rule): List[Rule] = rule match {
      case rule @ Rule(_, ((_: Terminal) :: _)) =>
        List(rule)
      case rule @ Rule(lhs, ((head: Nonterminal) :: _)) if ntIndex(lhs) <= ntIndex(head) =>
        List(rule)
      case rule =>
        //here, index(head) < index(lhs)
        //inline 'head' and repeat the above process
        inlineFirstNonterminal(ruleList, rule).flatMap(inlRule => orderRule(ruleList, inlRule))
    }    
    val (modrules, zrules) = nonterms.foldLeft((List[Rule](), List[Rule]())) {
      case ((prevNTrules, zrules), nt) =>
        val ntrules = grules.filter(_.leftSide == nt)
        val newrules = ntrules.flatMap(orderRule(prevNTrules, _))        
        //remove left recursion from the new rules if it exists
        //Here, we assume that the order of rules is preserved
        val (newNTRules, newZRules) = removeLeftRecursion(newrules, nt)
        (prevNTrules ++ newNTRules, zrules ++ newZRules)
    }
    //important: put the `zrules` before the `modrules` as its rhs may start with 
    //non-terminals defined in `modrules`
    val orderedRules = zrules ++ modrules

    if (opctx.debugGNFConversion){
      printDebugMessage("OrderedRules", orderedRules.mkString(","))      
    }
    
    //start from An and substitute the first nonterminal if it exists by its right sides
    //Assuming removeLeftRecursion does not change the order of rules
    val gnfRules = orderedRules.foldRight(List[Rule]())((rule, acc) => {
      val newrules = inlineFirstNonterminal(acc, rule)
      if (opctx.debugGNFConversion)
        printDebugMessage("InliningFirst", "(" + rule + ") --> " + newrules.mkString(","))
      newrules ++ acc
    })           
    
    //epsilon rule needs to be added  back  if it was in the original grammar
    val epsilonRule = Rule(g.start, List())
    val finalRules = if (g.rules.contains(epsilonRule)) {
      epsilonRule +: gnfRules
    } else
      gnfRules      

    //simplify the transformed grammar
    val simplifications = {
      import CNFConverter._
      removeUnreachableRules[T] _ andThen
        removeUnproductiveRules
    }
    val gnfGrammar = simplifications(Grammar[T](g.start, finalRules))
    //dumpGrammar("final grammar")(gnfGrammar)
    gnfGrammar
  } 
}