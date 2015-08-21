package grammar

import CFGrammar._
import EBNFGrammar._
import java.io._
import scala.io.StdIn

object GrammarWriter {

  def dumpPrettyGrammar(filename: String, g: Grammar) = {    
    dumpGrammar(filename, CFGrammar.renameAutoSymbols(g))
  }
  
  def dumpGrammar(file: File, g: BNFGrammar): Unit = {
    dumpFile(file, g)
  }
  def dumpGrammar(file: File, g: Grammar): Unit = {
    dumpFile(file, g)
  }
  def dumpFile(file: File, content: Any): Unit = {
    val pw = new PrintWriter(new FileOutputStream(file))
    pw.print(content)
    pw.flush()
    pw.close()
  }
  
  def dumpGrammar(filename: String, g: Grammar) {
    val fullname = filename + ".gram"
    dumpGrammar(new File(fullname), g)
    println("Dumped grammar to file: " + fullname)    
  }
  
}