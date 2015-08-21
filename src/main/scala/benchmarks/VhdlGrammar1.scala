package benchmarks

import grammar._
import grammar.CFGrammar._
import java.io._
import generators.GrammarBoundingHelper

object VhdlGrammar1 extends Benchmark {
  import GrammarReaders._
  
  def benchmarkName = "UCVhdlGrammar"
  def benchmarkSource = "http://tams-www.informatik.uni-hamburg.de/vhdl/tools/uc_vhdlgrammar.Z"
   
  val filename = "grammar-import/uc_vhdl-nlrec.gram"
  def ebnfGrammar = {
    GrammarReaders.readFromFile(filename)
  }
}