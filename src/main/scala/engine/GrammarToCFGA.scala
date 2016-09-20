package engine

import grammar.utils._
import grammar._
import java.io._
import java.lang.management._
import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer
import grammar.EBNFGrammar._
import grammar.CFGrammar._

object GrammarToCFGA {

  def grammarToCFGA(g: Grammar[String], filename: String) {            
    val pr = new PrintWriter(new FileOutputStream(new File(filename)))
    val st = g.start 
    val nts = g.start +: (g.nonTerminals.filterNot(_ == st))    
    val gramstr = nts.foldLeft(""){(acc, nt) =>
      acc + nt +
      g.nontermToRules(nt).foldLeft(""){(acc,rl) => 
       acc + " : " + rl.rightSide.foldLeft(""){
        case (acc, t : Terminal[String]) => acc + " \""+t+"\""
        case (acc, nt : Nonterminal) => acc + " "+ nt
      } + ";\n" 
    } + "\n" }
    
    val pw = new PrintWriter(new FileOutputStream(new File(filename)))
    pw.print(gramstr)
    pw.close()
  } 
}