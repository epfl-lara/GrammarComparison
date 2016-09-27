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
  def parseWithTrees(s: List[Terminal[T]])(implicit opctx: GlobalContext): Stream[ParseTree[T]]
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
case class Leaf[T](t: T) extends  NodeOrLeaf[T]

object ParseTreeDSL {
  def mapTree[T](p: ParseTree[T]): NodeOrLeaf[T] = {
    p match {
      case PNode(Rule(lhs, rhs), child) => 
        Node(::=(lhs.sym, rhs.map{ case Nonterminal(sym) => sym; case Terminal(obj) => obj }),
            child.map{ mapTree })
      case PLeaf(Terminal(t)) => Leaf(t)
    }
  }
}
case class ::=(lhs: scala.Symbol, rhs: List[Any]) {
  override def toString = lhs + " ::= " + rhs
}

object ParseTreeUtils {

  class TerminalWrapper[T](val t: Terminal[T]) {    
    val key  = t.obj match {
      case tc: TerminalClass => tc.terminalClass
      case _ => t
    }
    def compare(t: Terminal[T]) = t.obj match {
      case tc: TerminalClass => key == tc.terminalClass
      case _ => key == t
    }
    override def equals(obj: Any) = obj match {
      case tw : TerminalWrapper[T] => key == tw.key
      case _ => false
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

  def getParser[T](g: Grammar[T]) =
    if (GrammarUtils.isLL1(g)) {
      new LL1Parser(g)
    } else new CYKParser(g.cnfGrammar)

  def parse[T](g: Grammar[T], s: List[T])(implicit opctx: GlobalContext): Boolean = {
    val terms = s.map(Terminal[T] _)
    val parser = getParser(g)
    parser.parse(terms)
  }

  /*def parseWithTree[T](g: Grammar[T], s: List[T])(implicit opctx: GlobalContext): Option[ParseTree[T]] = {
    val terms = s.map(Terminal[T] _)
    val parser = getParser(g)
    parser.parseWithTree(terms)
  }*/

  def parseWithTrees[T](g: Grammar[T], s: List[T])(implicit opctx: GlobalContext): Stream[NodeOrLeaf[T]] = {
    val terms = s.map(Terminal[T] _)
    val parser = getParser(g)
    parser.parseWithTrees(terms).map(t => ParseTreeDSL.mapTree(t))
  }
}