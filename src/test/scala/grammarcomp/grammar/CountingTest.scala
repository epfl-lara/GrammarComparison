package grammarcomp
package grammar

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import CFGrammar._
import generators._
import grammar.examples.Korenjak1966
import grammar.examples.Olshansky1977

class CountingTest extends FlatSpec with Matchers {
  
  implicit val opctx = new GlobalContext(enableStats = false)
  implicit val enumctx = new EnumerationContext()
  
  import scala.language.implicitConversions
  implicit def strToSym(str: String): scala.Symbol = scala.Symbol(str) 
  
  "getMinWord" should " should work correctly for Olshansky1977" in {    
    val wordGen =  new SizeBasedRandomAccessGenerator(Olshansky1977.reference.cfGrammar, 10)    
    wordToString(wordGen.getMinWord(Nonterminal("S1")).get) should equal ("( )")    
    wordToString(wordGen.getMinWord(Nonterminal("D")).get) should equal (")")
    wordToString(wordGen.getMinWord(Nonterminal("B")).get) should equal (")")
  }
  
  "getMinWord" should " should work correctly for Korenjak1966" in {
    val wordGen =  new SizeBasedRandomAccessGenerator(Korenjak1966.reference.cfGrammar, 10)    
    wordToString(wordGen.getMinWord(Nonterminal("S1")).get) should equal ("a a b a")    
    wordToString(wordGen.getMinWord(Nonterminal("A")).get) should equal ("a b")
    wordToString(wordGen.getMinWord(Nonterminal("C")).get) should equal ("a")
  }     
}