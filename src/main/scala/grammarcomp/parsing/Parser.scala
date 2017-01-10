package grammarcomp

package parsing
import grammar._
import CFGrammar._
import utils.Util.MultiMap
import utils.Util.TrieMap
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.LinkedHashSet
import scala.collection.mutable.HashSet
import java.io._
import grammar.utils._
import scala.collection.immutable.Range

trait InternalFeedback[T]
class Parsed[T](val parseTrees: Stream[ParseTree[T]]) extends InternalFeedback[T]
case class CYKFeedback[T](cyktable: List[(Int, Int, Set[Nonterminal])]) extends InternalFeedback[T]
case class LLFeedback[T](nt: Nonterminal, char: Option[Terminal[T]]) extends InternalFeedback[T]

trait Parser[T] {
  def parse(s: List[Terminal[T]])(implicit opctx: GlobalContext): Boolean
  /**
   * Returns one parse tree of the string `s` if it is parsable.
   * The parse tree is chosen arbitrarily if the grammar is ambiguous
   */
  def parseWithTree(s: List[Terminal[T]])(implicit opctx: GlobalContext): Option[ParseTree[T]]
  /**
   * Returns all parse trees of the string `s` as a stream.
   */
  def parseWithTrees(s: List[Terminal[T]])(implicit opctx: GlobalContext): InternalFeedback[T]
}

/**
 * Parse trees associated to rules in CNF
 */
trait ParseTree[T]
case class PNode[T](r: Rule[T], children: List[ParseTree[T]]) extends ParseTree[T]
case class PLeaf[T](t: Terminal[T]) extends ParseTree[T]

/**
 * Functions for matching our parse trees conveniently
 */
sealed abstract class NodeOrLeaf[T] {
  override def toString = {
    val spaceUnit = 4
    var str = ""
    def recStr(ptree: NodeOrLeaf[T], indentLevel: Int) {
      val indent = " " * (indentLevel * spaceUnit)
      str += "\n" + indent
      ptree match {
        case Node(r, children) =>
          str += r.lhs
          children.foreach(recStr(_, indentLevel + 1))
        case Leaf(t) =>
          str += t
      }
    }
    recStr(this, 0)
    str
  }
}
case class Node[T](rule: ::=, children: List[NodeOrLeaf[T]]) extends NodeOrLeaf[T]
case class Leaf[T](t: T) extends NodeOrLeaf[T]

object ParseTreeDSL {
  def mapTree[T](p: ParseTree[T]): NodeOrLeaf[T] = {
    p match {
      case PNode(Rule(lhs, rhs), child) =>
        Node(::=(lhs.sym, rhs.map { case Nonterminal(sym) => sym; case Terminal(obj) => obj }),
          child.map { mapTree })
      case PLeaf(Terminal(t)) => Leaf(t)
    }
  }
}
case class ::=(lhs: scala.Symbol, rhs: List[Any]) {
  override def toString = lhs + " ::= " + rhs
}

trait ParseFeedback[T]
class Success[T](val parseTrees: Stream[NodeOrLeaf[T]]) extends ParseFeedback[T]
case class CYKError[T](cykTable: List[(Int, Int, Set[Nonterminal])]) extends ParseFeedback[T] {
  override def toString =
    "Substrings that are parsable by at least one nonterminal:\n" +
      cykTable.collect {
        case (i, j, nts) if !nts.isEmpty => s"d($i,$j) = { ${nts.mkString(",")} }"
      }.mkString("\n")
}
case class LL1Error[T](nt: Nonterminal, char: Option[Terminal[T]]) extends ParseFeedback[T] {
  override def toString = {
    char match {
      case Some(c) => s"No production of $nt applies to character: $c"
      case None    => s"No production of $nt applies to epsilon (End-of-stream)"
    }
  }
}

object ParseTreeUtils {

  class TerminalWrapper[T](val t: Terminal[T]) {
    val key = t.obj match {
      case tc: TerminalClass => tc.terminalClass
      case _                 => t
    }
    def compare(t: Terminal[T]) = t.obj match {
      case tc: TerminalClass => key == tc.terminalClass
      case _                 => key == t
    }
    override def equals(obj: Any) = obj match {
      case tw: TerminalWrapper[T] => key == tw.key
      case _                      => false
    }
    override def hashCode = key.hashCode()
  }

  def parseTreetoString[T](p: ParseTree[T]): String = {
    val spaceUnit = 4
    var str = ""
    def recStr(ptree: ParseTree[T], indentLevel: Int) {
      val indent = " " * (indentLevel * spaceUnit)
      str += "\n" + indent
      ptree match {
        case PNode(r, children) =>
          str += r.leftSide
          children.foreach(recStr(_, indentLevel + 1))
        case PLeaf(t) =>
          str += t
      }
    }
    recStr(p, 0)
    str
  }

  def dumpParseTreeToFile[T](p: ParseTree[T], filename: String) = {
    val pw = new PrintWriter(new FileOutputStream(new File(filename)))
    pw.print(parseTreetoString(p))
    pw.close()
  }

  def parse[T](g: Grammar[T], s: List[T])(implicit opctx: GlobalContext): Boolean = {
    val terms = s.map(Terminal[T] _)
    val parser =
      if (GrammarUtils.isLL1(g)) {
        //println("[Info] Using LL(1) Parsing Algorithm")
        new LL1Parser(g)
      } else {
        //println("[Info] Using CYK Parsing Algorithm")
        new CYKParser(g.cnfGrammar)
      }
    parser.parse(terms)
  }

  def parseWithTrees[T](g: Grammar[T], s: List[T])(implicit opctx: GlobalContext): ParseFeedback[T] = {
    val terms = s.map(Terminal[T] _)
    val parser =
      if (GrammarUtils.isLL1(g)) {
        //println("[Info] Using LL(1) Parsing Algorithm")
        new LL1Parser(g)
      } else {
        //println("[Info] Using CYK Parsing Algorithm")
        new CYKParser(g.twonfGrammar)
      }
    parser.parseWithTrees(terms) match {
      case p: Parsed[T]          => new Success(p.parseTrees.map(t => ParseTreeDSL.mapTree(t)))
      case CYKFeedback(fdb) => CYKError(fdb)
      case LLFeedback(nt, char)  => LL1Error(nt, char)
    }
  }
}