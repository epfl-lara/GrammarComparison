package grammarcomp

package benchmarks

import grammar._
import grammar.CFGrammar._
import java.io._
import generators.GrammarBoundingHelper

object AntlrVhdlGrammar extends Benchmark {
  import GrammarReaders._
  
  def benchmarkName = "AntlrVhdlGrammar"
  def benchmarkSource = "https://raw.githubusercontent.com/antlr/grammars-v4/master/vhdl/vhdl.g4"

  val filename = "antlr-grammars/vhdl-antlr.g4.gram"
  def ebnfGrammar = {
    GrammarReaders.readFromFile(filename)
  }
}

object AntlrVhdlGrammarCorrected extends Benchmark {
  import GrammarReaders._
  
  def benchmarkName = "AntlrVhdlGrammar"
  def benchmarkSource = "https://raw.githubusercontent.com/antlr/grammars-v4/master/vhdl/vhdl.g4"

  val filename = "antlr-grammars/vhdl-antlr.g4.2.gram"
  def ebnfGrammar = {
    GrammarReaders.readFromFile(filename)
  }
}