package benchmarks
import grammar._
import grammar.CFGrammar._
import java.io._
import generators.GrammarBoundingHelper

object AntlrPascalGrammar extends Benchmark {
  import GrammarReaders._

  def benchmarkName = "AntlrPascalGrammar"
  def benchmarkSource = "http://www.antlr3.org/grammar/1279217060704/pascal3.zip"
  val filename = "antlr-grammars/pascal.g.gram"
  def ebnfGrammar = {
    GrammarReaders.readFromFile(filename)
  }

  //val boundFilename = "java-antlr-bounded"  
  //lazy val boundedGrammar =  GrammarBoundingHelper.createBoundedGrammar(grammar,Some(1))
}