package grammarcomp

package benchmarks

import grammar._
import grammar.CFGrammar._
import java.io._
import generators.GrammarBoundingHelper

object Python3Grammar extends Benchmark {
 
  import GrammarReaders._
  
  def benchmarkName = "Python3Grammar"
  def benchmarkSource = "https://docs.python.org/3/reference/grammar.html"

  val filename = "grammar-import/python3.gram"
  def ebnfGrammar = {
    GrammarReaders.readFromFile(filename)
  }
}

object Python3GrammarAntlr extends Benchmark {
 
  import GrammarReaders._
  
  def benchmarkName = "Python3GrammarAntlr"
  def benchmarkSource = "https://github.com/antlr/grammars-v4/blob/master/python3/Python3.g4"

  val filename = "antlr-grammars/python3.g4.gram"
  def ebnfGrammar = {
    GrammarReaders.readFromFile(filename)
  }
}