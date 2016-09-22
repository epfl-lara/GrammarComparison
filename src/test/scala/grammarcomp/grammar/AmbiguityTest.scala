package grammarcomp
package grammar

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import grammar.examples.Olshansky1977
import clients.AmbiguityChecker

class AmbiguityTest extends FlatSpec with Matchers {

  implicit val gctx = new GlobalContext()
  implicit val opctx = new AmbiguityContext()
  implicit val ectx = new EnumerationContext()

  "AmbiguityChecker.checkAmbiguityInStudentGrammars" should " word correctly" in {
    val grammar = examples.IfThenElse.reference.cfGrammar       
    val res1 = new AmbiguityChecker(grammar).checkAmbiguityInStudentGrammar()
    res1.isEmpty should be(false)
    
    val res2 = new AmbiguityChecker(Olshansky1977.reference.cfGrammar).checkAmbiguityInStudentGrammar()
    res2.isEmpty should be(true)
  }    
}