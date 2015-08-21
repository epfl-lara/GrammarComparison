package parsing

import grammar.utils._
import grammar._
import java.io._
import grammar.EBNFGrammar._
import grammar.CFGrammar._

/*object GrammarToAntlr {

  def grammarToAntlr(g: Grammar, gname: String): String = {
    
    def  rsideToStr(rside: List[Symbol]) = {      
      rside.foldLeft("") {
          case (acc, t: Terminal) => acc + " '" + t + "'"
          case (acc, nt: Nonterminal) => acc + " " + nt
        } 
    }
    
    //first add a name for the grammar
    val antlrStr = "grammar " + gname + ";\n"
    val st = g.start
    val nts = g.start +: (g.nonTerminals.filterNot(_ == st))
    nts.foldLeft(antlrStr) { (acc, nt) =>
      val rules = g.nontermToRules(nt) 
      //make the empty rule the last rule
      val (head :: tail) = if(rules.exists(_.rightSide.isEmpty)){
        rules.filterNot(_.rightSide.isEmpty) :+ Rule(nt,List())
      } else 
        rules           
        
      val firstLine = acc + nt + " : " + rsideToStr(head.rightSide) + "\n"
      tail.foldLeft(firstLine) { (acc, rl) =>
        acc + " \t| " + rsideToStr(rl.rightSide) + "\n"
      } + "\t;\n"
    }
  }
}*/