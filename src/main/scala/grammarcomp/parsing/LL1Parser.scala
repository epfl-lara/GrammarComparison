package grammarcomp

package parsing

import grammar.CFGrammar._
import scala.annotation.tailrec
import grammar.GrammarUtils
import grammar.GlobalContext
import ParseTreeUtils._

class LL1Parser[T](g: Grammar[T]) extends Parser[T] {
  case class EndOfRule(r: Rule[T])

  def parse(s: List[Terminal[T]])(implicit opctx: GlobalContext): Boolean = {
    parseWithTree(s).nonEmpty
  }
  //Returns one possible grammar list
  def parseWithTree(s: List[Terminal[T]])(implicit opctx: GlobalContext): Option[ParseTree[T]] = {
    require(GrammarUtils.isLL1(g))

    val (nullable, first, follow) = GrammarUtils.nullableFirstFollow(g)
    //TODO: Compute the parsing table: Depending on the symbol we parse, which is the rule that we are going to apply.
    val nonTerminals = g.nonTerminals
    val parseTableBuild = for (
      nt <- nonTerminals;
      rule <- g.nontermToRules(nt);
      t <- GrammarUtils.firstA(rule.rightSide, nullable, first) ++ (if (rule.rightSide.forall(nullable)) follow(nt) else Nil)
    ) yield (nt, t, rule)

    val parseTable: Map[Nonterminal, List[(Terminal[T], Rule[T])]] = parseTableBuild.groupBy(_._1).map {
      case (k, vals) => k -> vals.map { case (_, t, rule) => (t, rule) }
    }.toMap

    // map all non-terminals to their nullable righthand side
    val nullableRHS = g.nontermToRules.map {
      case (nt, rules) => (nt -> rules.find(_.rightSide.forall(nullable)))
    }
    /**
     * Picks a rule to use based on the input terminal
     */
    def findRule(nt: Nonterminal, inptOpt: Option[Terminal[T]]): Option[Rule[T]] = {
      parseTable.get(nt) match {
        case None => None
        case Some(l) =>
          inptOpt match {
            case Some(inpt) =>
              l.collectFirst {
                case (t, rule) if compareTerminal(t, inpt) =>
                  //println(s"foundRule for terminal: $inpt rule: $rule")
                  rule
              }
            case None => // the symbol is epsilon
              nullableRHS(nt)
          }
      }
    }

    @tailrec def rec(current: List[Either[Symbol[T], EndOfRule]], s: List[Terminal[T]], acc: List[ParseTree[T]]): Option[ParseTree[T]] = (current, s) match {
      case (Nil, Nil) => acc.headOption
      case (Right(EndOfRule(rule)) :: q, s) =>
        val n = rule.rightSide.length
        rec(q, s, Node(rule, acc.take(n).reverse) :: acc.drop(n))
      case (Left(nt: Nonterminal) :: q, l) => findRule(nt, l.headOption) match { // also handles the end-of-stream case
        case Some(rule) =>
          rec(rule.rightSide.map(Left.apply) ++ List(Right(EndOfRule(rule))) ++ q, s, acc)
        case _ => None
      }            
      case (Left(t: Terminal[T]) :: q, a :: b) if compareTerminal(t, a) =>
        rec(q, b, Leaf(a) :: acc)     
      case _ =>
        None
    }
    rec(Left(g.start) :: Nil, s, Nil)
  }

  def parseWithTrees(s: List[Terminal[T]])(implicit opctx: GlobalContext) = parseWithTree(s) match {
    case Some(t) => Stream(t)
    case _       => Stream.empty
  }
}