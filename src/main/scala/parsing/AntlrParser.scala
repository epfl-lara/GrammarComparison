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

/**
 * As of now this parser can be used only with grammar where terminals are strings.
 * Because translation to antlr syntax requires conversion to strings.
 */
class AntlrParser(g: Grammar[String])(implicit opctx: ParseContext) extends Parser[String] {

  var lexerClass: Class[_] = null
  var parserClass: Class[_] = null
  var lexerCons: Constructor[_] = null
  var parserCons: Constructor[_] = null

  val gname = "G" + Util.freshNumber //create a fresh grammar name

  val antlrg = {
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

    renameAutoSymbols(replace(ng, replaceMap))
  }

  //for debugging
  //  /GrammarWriter.dumpGrammar("temp", antlrg)

  compileGrammarToParser()

  def compileGrammarToParser() {
    //(a) convert the grammar to antlr format.
    val dir = opctx.antlrCompilationDir + "/"
    val classpath = opctx.antlrJarPath
    val antlrfn = dir + gname + ".g4"
    val pr = new PrintWriter(new FileOutputStream(new File(antlrfn)))
    pr.println(grammarToAntlr(gname))
    pr.flush()
    pr.close()

    //(b) compile the antlr grammar to a parser and lexer    
    val antlrTool = new Tool(Array[String](antlrfn))
    antlrTool.processGrammarsOnCommandLine();
    if (antlrTool.errMgr.getNumErrors() > 0) {
      throw new IllegalStateException("Errors in generating parsers !!")
    }

    //(c) generate class files from the parser and lexer
    val compiler = ToolProvider.getSystemJavaCompiler();

    val files = List(gname + "Lexer.java", gname + "Parser.java", gname + "BaseListener.java", gname + "Listener.java")
    val javaFiles = files.map(fn => new File(dir + fn))
    /*
       * val folder = new File(dir)
       * folder.list().collect{
      case fn if(fn.endsWith(".java")) => 
        new File(dir+fn)
    }*/
    val fileManager = compiler.getStandardFileManager(null, null, null);
    val compilationUnits =
      fileManager.getJavaFileObjectsFromFiles(javaFiles.toList);
    //set class path
    val optionList = List("-classpath", classpath)
    val compileRes = compiler.getTask(null, fileManager, null, optionList, null, compilationUnits).call()
    if (!compileRes) {
      cleanup
      throw new IllegalStateException("Errors in compiling Antlr code!!")
    }

    //(d) load the parsers using reflection
    lexerClass = Class.forName(gname + "Lexer") //.asInstanceOf[Class[Lexer]]
    parserClass = Class.forName(gname + "Parser") //.asInstanceOf[Class[Parser]]    
    //println("Constructors in the lexer class: ")    
    //lexerClass.getConstructors().foreach(m =>println(m.getName()+"/"+m.getParameterTypes().map(_.getName()).mkString(",")))
    // println("Constructors in the parser class: ")    
    //parserClass.getConstructors().foreach(m =>println(m.getName()+"/"+m.getParameterTypes().map(_.getName()).mkString(",")))
    lexerCons = lexerClass.getConstructors()(0)
    println(lexerCons.getName() + "/" + lexerCons.getParameterTypes().map(_.getName()).mkString(","))
    parserCons = parserClass.getConstructors()(0)
    println(parserCons.getName() + "/" + parserCons.getParameterTypes().map(_.getName()).mkString(","))
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

  def parse(s: List[Terminal[String]])(implicit gctx: GlobalContext): Boolean = {
    //for stats
    gctx.stats.updateCounter(1, "AntlrParseCalls")
    val timer = new Stats.Timer()

    val input = new ANTLRInputStream(s.mkString(" "));
    val lexer = lexerCons.newInstance(input).asInstanceOf[Lexer]
    lexer.removeErrorListeners();

    val tokenStream = new CommonTokenStream(lexer)
    val parser = parserCons.newInstance(tokenStream).asInstanceOf[org.antlr.v4.runtime.Parser]
    parser.removeErrorListeners()

    val method = parserClass.getMethod(antlrg.start.toString) //get the method corresponding to the root    
    val tree = method.invoke(parser).asInstanceOf[ParserRuleContext]
    //System.out.println(res.toStringTree());
    //println(parser.getNumberOfSyntaxErrors)

    //for stats
    gctx.stats.updateCounterTime(timer.timeWithoutGC(), "AntlrParseTime", "AntlrParseCalls")

    (parser.getNumberOfSyntaxErrors == 0)
  }

  def parseWithTree(s: List[Terminal[String]])(implicit gctx: GlobalContext) = {
    throw new IllegalStateException("Not implemented yet!")
  }
  
  def parseWithTrees(s: List[Terminal[String]])(implicit gctx: GlobalContext) = {
    throw new IllegalStateException("Not implemented yet!")
  }

  def cleanup() {
    val dir = opctx.antlrCompilationDir + "/"
    (new java.io.File(dir)).listFiles().filter(_.getName().startsWith(gname)).foreach { fl =>
      try { fl.delete() }
      catch { case e: Exception => ; } //ignore the exception 
    }
  }
}