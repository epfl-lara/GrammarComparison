package grammarcomp

package benchmarks
import grammar._
import grammar.CFGrammar._
import java.io._
import generators.GrammarBoundingHelper


object SatPaperPascalGrammar extends Benchmark {
  import GrammarReaders._
  
  def benchmarkName = "SatPaperPascalGrammar"
  def benchmarkSource = "ftp://ftp.iecc.com/pub/file/pascal-grammar" 
  val filename = "grammar-import/cfga-pascal.gram"
  def ebnfGrammar = {
    GrammarReaders.readFromFile(filename)
  }
  
  //val boundFilename = "satpaper-pascal-bounded"  
  //lazy val boundedGrammar =  GrammarBoundingHelper.createBoundedGrammar(grammar,None)
}