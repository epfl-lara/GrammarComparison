package grammarcomp
package grammar

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import CFGrammar._
import grammarcomp.parsing._
import GrammarDSL._

class LL1Test extends FlatSpec with Matchers {
  import GrammarReaders._

  implicit val gctx = new GlobalContext()
  implicit val opctx = new ParseContext()

  import scala.language.implicitConversions
  implicit def strToSym(str: String): scala.Symbol = scala.Symbol(str)
  
  "LL1 parser" should "parse correctly" in {
    val grammar = Grammar('S, List[Rules[String]](
        'S ::= "a" ~ "b" ~ "c"
    ))
    val parser = new LL1Parser(grammar)
    val EP = new EarleyParser(grammar)
    
    val s = ("a c c".split(" ") map Terminal.apply _).toList
    val s2 = ("a b c".split(" ") map Terminal.apply _).toList
    
    val res = parser.parse(s)
    val res2 = parser.parse(s2)
    
    res should be(false)
    res2 should be(true)
  }
}