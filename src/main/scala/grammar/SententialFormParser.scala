package grammar

import scala.util.parsing.combinator.RegexParsers
import CFGrammar._
import EBNFGrammar._
import language.postfixOps

/**
 * Parses a sentential form belonging to a grammar.
 * Note to parse a word belonging to a grammar we can use CYK parser
 */
class SententialFormParser extends RegexParsers {
  override type Elem = Char
  
  def oneSymbol(sym: Symbol) = sym match {
    case t@Terminal(name) => name ^^ { case _ => t}
    case nt@Nonterminal(name) => name ^^ { case _ => nt}  
  }
  
  def anySymbol(g: Grammar) = {       
    val syms = (g.nonTerminals ++ g.terminals)
    syms.tail.foldLeft(oneSymbol(syms.head) : Parser[Symbol]) {
      case (acc, sym) =>
        acc | oneSymbol(sym)
    }
  }

  def sform(g: Grammar) = anySymbol(g)*

  /**
   * Parses the steps of a leftmost derivation for a given grammar
   */
  def parseSententialForms(lines: List[String], g: Grammar) = {

    var linenum = 0
    var errstr = "";
    val steps = lines.foldLeft(List[SententialForm]())((acc, line) => {
      linenum = linenum + 1
      if (!errstr.isEmpty)
        acc
      else if (line.trim().isEmpty())
        //ignore this line and goto the next one
        acc
      else {
        parseAll(sform(g), line) match {
          case Success(sform, _) =>            
            acc :+ sform
          case NoSuccess(err, input) =>
            errstr += ("failed to parse input " +
              "(line " + linenum + ", column " + input.pos.column + "):\n" +
              err + "\n" +
              input.pos.longString)
            acc
        }
      }
    })
    if (errstr.isEmpty() && steps.isEmpty)
      (List(), "Input is empty!")
    else{      
      (steps, errstr)
    }
  }
}