package grammarcomp

package benchmarks
import grammar._
import grammar.CFGrammar._
import java.io._
import generators.GrammarBoundingHelper

object AntlrJavaGrammar extends Benchmark{
  import GrammarReaders._
  def benchmarkName = "AntlrJavaGrammar"
  def benchmarkSource = "https://github.com/antlr/grammars-v4"
    
  val filename = "antlr-grammars/Java-antlr.gram"
  def ebnfGrammar = {
    GrammarReaders.readFromFile(filename)
  }
  
  //val boundFilename = "java-antlr-bounded"  
  //lazy val boundedGrammar =  GrammarBoundingHelper.createBoundedGrammar(grammar,Some(1))
}