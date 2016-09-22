package grammarcomp

package parsing

import grammar.CFGrammar._
import scala.annotation.tailrec
import grammar.GrammarUtils
import java.io._
import grammar._
import grammar.utils._
import org.antlr.v4._
import org.antlr.v4.runtime._
import javax.tools.ToolProvider
import scala.collection.JavaConversions._
import org.antlr.runtime.ANTLRStringStream
import java.lang.reflect.Constructor
import org.antlr.runtime.tree.CommonTree

class AntlrParserInterpreted(ing: Grammar[String])
	(implicit gctx: GlobalContext, opctx: ParseContext) extends Parser[String] {

  val antlrg = {
    //check if there is indirect left recursion. If yes, convert the grammar to GNF
    val g = convertGrammarForAntlrParsing(ing)
    //create a new start symbol    
    val ns = CFGrammar.freshNonterminal(Some(g.start.name))
    val nrs = Rule(ns, List[Symbol[String]](g.start))
    val ng = Grammar[String](ns, nrs +: g.rules)

    //make non-terminals start with lower case letters, if they don't already
    val oldnts = (ng.nonTerminals.map(_.name) ++ ng.terminals.map(_.toString)).toSet
    val replaceMap = ng.nonTerminals.collect {
      case ont : Nonterminal if (ont.name(0).isUpper) =>
        val newname = ont.name(0).toLower + ont.name.substring(1)
        val nnt =
          if (oldnts.contains(newname))
            //add a suffix to the newname
            Nonterminal(scala.Symbol(newname + Util.freshNumber))
          else
            Nonterminal(scala.Symbol(newname))
        (ont -> nnt)
    }.toMap

    val fg = renameAutoSymbols(replace(ng, replaceMap))
    
    if(opctx.debugAntlrInterpreter)
    	println(s"Grammar size: (${fg.nonTerminals.size},${fg.rules.size})") 
    fg
  }
  //dump the grammar to a file (we are not compiling here)
  val antlrGrammar = {
    val gname = "G" + Util.freshNumber //create a fresh grammar name
    val antlrstr = grammarToAntlr(gname)
    if(opctx.debugAntlrInterpreter){
      val pw = new PrintWriter(new FileOutputStream(new File(gname+".g4")))
      pw.println(antlrstr)
      pw.close()
      println("Outputted to file: "+gname)
    }    
    new org.antlr.v4.tool.Grammar(antlrstr)
  }

  def convertGrammarForAntlrParsing(ig: Grammar[String]): Grammar[String] = {
    //first remove unproductive and unreachable symbols
    val g = CNFConverter.simplify(ing)
    //(a) there should be no indirect left recursion   
    val nullables = GrammarUtils.nullables(g)
    val sccs = GNFUtilities.indirectLeftRecursiveNonterms(g)
    if (sccs.exists(_.size >= 2)) {
      //there is an indirect left recursion, put this in GNF form
      GNFConverter.toGNF(g)
    } else {
      //no indirect self recursion i.e, S -> A S, where A is nullable
      val foundViolation = sccs.flatten.toSet.exists { nt =>
        g.nontermToRules(nt).exists {
          case Rule(l, r @ (first :: rest)) if first != l && rest.contains(l) =>
            val findex = r.indexOf(l)
            r.take(findex).forall {
              case n: Nonterminal => nullables(n)
              case _ => false
            }
          case _ => false
        }
      }
      if (foundViolation) {
        GNFConverter.toGNF(g)
      } else {
        //(b) left recursive non-terminals (in left recursive rules) should not be followed by a nullable symbol
        val leftrecRules = g.rules.filter {
          case Rule(l, first :: rest) => l == first
          case _ => false
        }
        val foundViolation = leftrecRules.exists {
          case Rule(l, first :: (second: Nonterminal) :: rest) if (nullables(second)) => true
          case _ => false
        }
        if (foundViolation)
          GNFConverter.toGNF(g)
        else {
          //(c) left recursive non-terminals should have at least one non-left recursive alternative          
          g
        }
      }
    }
  }

  def grammarToAntlr(gname: String) = {

    val tg = antlrg
    def rsideToStr(rside: List[Symbol[String]]) = {
      rside.foldLeft("") {
        case (acc, t: Terminal[String]) => acc + " '" + t + "'"
        case (acc, nt: Nonterminal) => acc + " " + nt
      }
    }

    //first add a name for the grammar
    val nts = tg.start +: (tg.nonTerminals.filterNot(_ == tg.start))
    val antlrStr = nts.foldLeft("grammar " + gname + " ;\n") { (acc, nt) =>
      val rules = tg.nontermToRules(nt)
      //make the empty rule the last rule
      val (head :: tail) = if (rules.exists(_.rightSide.isEmpty)) {
        rules.filterNot(_.rightSide.isEmpty) :+ Rule(nt, List())
      } else
        rules

      val firstLine = acc + nt + " : " + rsideToStr(head.rightSide) + (
        if (nt == tg.start) " EOF\n" else "\n")
      tail.foldLeft(firstLine) { (acc, rl) =>
        acc + " \t| " + rsideToStr(rl.rightSide) + "\n"
      } + "\t;\n"
    }
    //add command to skip whitespaces and handle all tokens in the input
    antlrStr + "WS : [ \\t\\r\\n]+ -> skip ;" + "\nErrorChar : . ;"
  }

  def parse(s: List[Terminal[String]])(implicit opctx: GlobalContext): Boolean = {
    //for stats
    opctx.stats.updateCounter(1, "AntlrParseCalls")
    val timer = new Stats.Timer()

    val input = new ANTLRInputStream(s.mkString(" "));
    val lexEngine = antlrGrammar.createLexerInterpreter(input);
    val tokenStream = new CommonTokenStream(lexEngine)
    val parser = antlrGrammar.createParserInterpreter(tokenStream)
    parser.removeErrorListeners()    
    val tree = parser.parse(antlrGrammar.getRule(antlrg.start.name).index)
    //System.out.println(tree.toStringTree())
    //println(parser.getNumberOfSyntaxErrors)                

    //for stats
    val time = timer.timeWithoutGC()
    opctx.stats.updateCounterTime(time, "AntlrParseTime", "AntlrParseCalls")
    if(time >= opctx.stats.getMax("AntlrParseTime")){
      opctx.stats.updateVariable("MaxParseTimeString", wordToString(s))
    }
    
    (parser.getNumberOfSyntaxErrors == 0)
  }

  def parseWithTree(s: List[Terminal[String]])(implicit opctx: GlobalContext): Option[ParseTree[String]] = {
    throw new IllegalStateException("Not implemented yet!")
  }
  
  def parseWithTrees(s: List[Terminal[String]])(implicit gctx: GlobalContext) = {
    throw new IllegalStateException("Not implemented yet!")
  }
}