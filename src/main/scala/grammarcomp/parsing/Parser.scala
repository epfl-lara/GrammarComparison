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
case class Node[T](r: Rule[T], children: List[ParseTree[T]]) extends ParseTree[T]
case class Leaf[T](t: Terminal[T]) extends ParseTree[T]

object ParseTreeUtils {

  def parseTreetoString[T](p: ParseTree[T]): String = {
    val spaceUnit = 4
    var str = ""
    def recStr(ptree: ParseTree[T], indentLevel: Int) {
      val indent = " " * (indentLevel * spaceUnit)
      str += "\n" + indent
      ptree match {
        case Node(r, children) =>
          str += r.leftSide
          children.foreach(recStr(_, indentLevel + 1))
        case Leaf(t) =>
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
  
  def parseWithTree[T](g: Grammar[T], s: List[T])(implicit opctx: GlobalContext): Option[ParseTree[T]] = {
    val terms = s.map(Terminal[T] _)
    val parser = getParser(g)      
    parser.parseWithTree(terms)   
  }
  
  def parseWithTrees[T](g: Grammar[T], s: List[T])(implicit opctx: GlobalContext): Stream[ParseTree[T]] = {
    val terms = s.map(Terminal[T] _)
    val parser = getParser(g)      
    parser.parseWithTrees(terms)   
  } 
}