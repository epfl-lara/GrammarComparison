package grammar

import grammar._
import grammar.CFGrammar._
import grammar.utils._

object GrammarUtils {

  def nullables[T](g: Grammar[T]): Set[Nonterminal] = {
    var nullable = Set.empty[Nonterminal]
    val nonTerminals = g.nonTerminals 
    Util.fixpoint {
      (nullable: Set[Nonterminal]) =>
        nullable ++ (for (
          X <- nonTerminals if !nullable(X) &&
            ((g.rules.exists { case Rule(X, Nil) => true case _ => false })
              ||
              (g.rules.exists {
                case Rule(X, l) if l.forall {
                  case n: Nonterminal => nullable(n)
                  case n: Terminal => false
                } => true case _ => false
              }))
        ) yield X)
    }(Set.empty[Nonterminal])
  }

  def nullableFirstFollow[T](g: Grammar[T]) 
  	: (Set[Symbol], Map[Symbol, Set[Terminal]], Map[Symbol, Set[Terminal]]) = {
    
    val nonTerminals = g.rules.map(_.leftSide).distinct
    val terminals = g.rules.flatMap(_.rightSide).distinct.collect { case t: Terminal => t }

    Util.fixpoint[(Set[Symbol], Map[Symbol, Set[Terminal]], Map[Symbol, Set[Terminal]])] {
      case (nul: Set[Symbol],
        fst: Map[Symbol, Set[Terminal]],
        fol: Map[Symbol, Set[Terminal]]) => {
        var nullable = nul
        var first = fst
        var follow = fol

        for (Rule(x, y) <- g.rules; k = y.size) {
          if (k == 0 || y.forall { case t: Nonterminal => nullable(t) case _ => false })
            nullable = nullable + x
          for (i <- 1 to k) {
            if (i == 1 || y.take(i - 1).forall { case t: Nonterminal => nullable(t) case _ => false })
              first += x -> (first(x) ++ first(y(i - 1)))
            if (i == k || y.drop(i).forall { case t: Nonterminal => nullable(t) case _ => false }) {
              follow += y(i - 1) -> (follow.getOrElse(y(i - 1), Set()) ++ follow(x))
            }
            for (j <- (i + 1) to k) {
              if (i + 1 == j || y.drop(i).take(j - i - 1).forall { case t: Nonterminal => nullable(t) case _ => false })
                follow += y(i - 1) -> (follow.getOrElse(y(i - 1), Set()) ++ first(y(j - 1)))
            }
          }
        }
        (nullable, first, follow)
      }
    }((Set.empty,
      Map[Symbol, Set[Terminal]]() ++ (for (X <- nonTerminals) yield (X: Symbol) -> Set[Terminal]()) ++ (for (Y <- terminals) yield (Y: Symbol) -> Set[Terminal](Y)),
      Map[Symbol, Set[Terminal]]() ++ (for (X <- nonTerminals) yield (X: Symbol) -> Set[Terminal]())))
  }

  def firstA(x: List[Symbol], nullable: Set[Symbol], fst: Map[Symbol, Set[Terminal]]): Set[Terminal] = {
    x match {
      case Nil => Set.empty
      case (a: Nonterminal) :: b if !nullable(a) => fst(a)
      case (a: Terminal) :: b => Set(a)
      case a :: b => fst(a) ++ firstA(b, nullable, fst)
    }
  }

  /**
   * Checks if a grammar is in LL(1)
   */
  def isLL1[T](g: Grammar[T]): Boolean = {     
    (isLL1WithFeedback(g) == InLL1())        
  }

  /**
   * Checks if a grammar is in LL(1) and provides feedback if not.
   * If the grammar is in LL(1), returns nothing
   */
  sealed abstract class LL1Feedback
  case class InLL1() extends LL1Feedback {
    override def toString = "The grammar is LL(1)"
  }

  case class IntersectingFirst(r1: Rule, r2: Rule, commonTerms: List[Terminal]) extends LL1Feedback {
    override def toString = {
      s"First($r1) \u2229 First($r2) = " + Util.setString(commonTerms)
    }
  }
  case class IntersectingFirstFollow(x: Nonterminal, commonTerms: List[Terminal]) extends LL1Feedback {
    override def toString = {
      s"$x is nullable and First($x) \u2229 Follow($x) = " + Util.setString(commonTerms)
    }
  }
  case class ManyNullables(r1: Rule, r2: Rule) extends LL1Feedback {
    override def toString = {
      val nt = r1.leftSide
      s"$nt has more than one nullable productions: $r1 , $r2"
    }
  }

  def isLL1WithFeedback[T](g: Grammar[T]): LL1Feedback = {
    val (nullable, first, follow) = nullableFirstFollow(g)
    val nonTerminals = g.nonTerminals

    //check if a nullable nonterminal has more than one nullable production
    for (X <- nullable) {
      X match {
        case nt: Nonterminal =>
          val nullableRules = g.nontermToRules(nt).collect {
            case rl @ Rule(_, l) if l.forall {
              case n: Nonterminal => nullable(n)
              case n: Terminal => false
            } => rl
          }
          if (nullableRules.size > 1)
            return ManyNullables(nullableRules(0), nullableRules(1))
        case _ => ;
      }
    }

    for (X <- nonTerminals) {
      val firsts = g.rules.collect { case r @ Rule(X, y) => (r, firstA(y, nullable, first)) }.zipWithIndex
      for (((r1, si), i) <- firsts) {
        for (((r2, sj), j) <- firsts if j > i) {
          val intersection = si.intersect(sj)
          if (!intersection.isEmpty) {
            //return Some(s"Rule $r1 where (first = $si) and rule $r2 where (first = $sj) have the terminal ${intersection.head} in common so the grammar is not LL(1)")
            return IntersectingFirst(r1, r2, intersection.toList)
          }
        }
      }
      if (nullable(X)) {
        val fst = first(X)
        val snd = follow(X)
        val intersection = fst.intersect(snd)
        if (intersection.nonEmpty)
          //return Some(s"Non-terminal $X for which (first = $fst, follow = $snd) is nullable but first and follow have an element in common: ${intersection.head}")
          return IntersectingFirstFollow(X, intersection.toList)
      }
    }
    InLL1()
  }

  def postOrder[T](g: Grammar[T]): List[Nonterminal] = {
    var visited = Set[Nonterminal](g.start)
    var order = List[Nonterminal]()

    def postRec(nt: Nonterminal): Unit = {
      //here, we cannot assume that unit productions have been removed
      if (g.nontermToRules.contains(nt)) {
        g.nontermToRules(nt).foreach(_.rightSide.foreach {
          case nextNt: Nonterminal if (!visited.contains(nextNt)) =>
            visited += nextNt
            postRec(nextNt)
          case _ =>
            ; //do nothing here
        })
      }
      //here add 'nt' to the order
      order :+= nt
    }
    postRec(g.start)
    order
  }

  def join(l: Seq[String], separator: String): String = {
    if (l.isEmpty) ""
    else if (l.tail.isEmpty) l.head
    else l.head + separator + join(l.tail, separator)
  }

  def clean(name: String): String = {
    "^_".r.replaceFirstIn(name.replace("=", "_Eq")
      .replace("<", "_Less")
      .replace(">", "_Greater")
      .replace("%", "_Mod")
      .replace("^", "_Xor")
      .replace("+", "_Plus")
      .replace("-", "_Minus")
      .replace("*", "_Times")
      .replace("/", "_Div")
      .replace("\"", "_DblQuote")
      .replace("'", "_Quote")
      .replace("|", "_Bar")
      .replace("!", "_Bang")
      .replace(".", "_Dot")
      .replace("[", "_LSquare")
      .replace("]", "_RSquare")
      .replace("‘", "_QuoteL")
      .replace("`", "_Tick")
      .replace("’", "_QuoteR")
      .replace("&", "_And")
      .replace(":", "_Colon")
      .replaceAll(" ", ""), "")
  }

  def createAccFile[T](g: Grammar[T]): String = {
    val tokens = (for (terminal <- g.terminals if terminal.toString.length > 1 || terminal.toString == "\"") yield terminal -> ("T" + clean(terminal.toString))).toMap

    var result = "%token " + join(tokens.values.toSeq, ", ") + ";\n";

    val nonterminalmap = (for (nt <- g.nonTerminals) yield nt -> clean(nt.name + nt.id).replace("_", "")).toMap

    result += "%nodefault\nRoot: " + nonterminalmap(g.start) + ";\n";
    result += "empty: ;\n";
    for (nontermRules <- g.nontermToRules) {
      val nonterm = nontermRules._1
      val rules = nontermRules._2

      result += nonterminalmap(nonterm) + " : " +
        join(rules.map(rule => if (rule.rightSide.isEmpty) "empty" else join(rule.rightSide.map { case s: Terminal => tokens.getOrElse(s, "'" + s.toString + "'") case n: Nonterminal => nonterminalmap(n) case s => s.toUniqueString }, " ")), " | ") + " ;\n";
    }
    result
  }

  import grammar._
  import BNFConverter._
  import grammar.examples._
  import java.io._
  import EBNFGrammar._
  def export(c: BNFGrammar[String], name: String): Unit = {
    export(ebnfToGrammar(c), name)
  }
  def export[T](c: Grammar[T], name: String): Unit = {
    def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
      val p = new java.io.PrintWriter(f)
      try { op(p) } finally { p.close() }
    }
    printToFile(new File(name))(p => p.println(createAccFile(c)))
  }
  // Sample usage
  // sbt console
  // grammar.GrammarUtils.export(benchmarks.JavaGrammar.reference, "JavaGrammar.acc")
}