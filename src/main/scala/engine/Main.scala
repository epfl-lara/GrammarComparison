package engine

import grammar.examples._
import grammar.utils._
import grammar._
import repair._
import parsing._
import CFGrammar._
import EBNFGrammar._
import BNFConverter._
import equivalence._
import generators._
import grammar.exercises._
import EvaluateQuiz._
import clients._
import java.io._
import generators.RandomAccessGenerator.Element
import scala.collection.mutable.ListBuffer
import benchmarks._
import AmbiguityChecker._
import scala.concurrent.duration._
import scala.concurrent.util._
import java.util.concurrent._
import GrammarReaders._
import GrammarDSL._
import EBNFGrammar._        

object Main {

  def generateUsingRAGenerator[T](g: Grammar[T]) = {

    implicit val gctx = new GlobalContext()
    implicit val enumctx = new EnumerationContext()

    val maxsize = 50
    val raenum = new SizeBasedRandomAccessGenerator(g.fromCNF, maxsize)
    val size = maxsize

    //for (size <- 1 to maxsize) {

    val spaceSize = raenum.wordCounter.boundForNonterminal(g.fromCNF.start, size)
    println("# words of size " + size + " : " + spaceSize)
    println("# cnf nonterms: " + g.cnfGrammar.nonTerminals.size + " # cnf rules: " + g.cnfGrammar.rules.size)

    val rand = new java.util.Random
    val rangeBits = spaceSize.bitLength - 1
    val samples = 100000
    val consec = false
    var i = 0
    var break = false

    //val antlrParser = new parsing.AntlrParser(AntlrJavaGrammar.ebnfGrammar.cfGrammar)
    while (i < samples & !break) {
      //sampling uniformly from the space
      val index = if (consec) BigInt(i) else BigInt(new java.math.BigInteger(rangeBits, rand))
      //val index = i
      //println("word #: " + index)

      gctx.stats.updateCounter(1, "WordGenCalls")
      raenum.getWordAtIndex(g.start, size, index) match {
        case Element(w) =>
          /*if (antlrParser.parse(w)) {
            println("Accepted word: " + w)
          } else
            throw new IllegalStateException("Rejected word: " + w)*/
          //            if (w.size != size)
          //              throw new IllegalStateException("Word size is not equal to size: " + w)
          if (index % 9797 == 0) //one in 9797 words
            println(index + "th word " + wordToString(w))

        case _ =>
          //println("No word at index: " + index)
          break = true
      }
      i += 1
    }
  }

  def main(args: Array[String]): Unit = {
    implicit val gctx = new GlobalContext()
    implicit val pctx = new ParseContext()
    implicit val ectx = new EnumerationContext()
    implicit val eqctx = new EquivalenceCheckingContext()

    val timer = new Stats.Timer()
    if (args.length < 1)
      throw new IllegalStateException("No option provided")
    val option = args(0)
    option match {
      case "-equivExpts" =>
        val dir = args(1)
        equivExpts(dir)

      case "-regr" =>
        //for safety
        Util.registerShutdownHook { cleanup _ }

        val dir = args(1)
        val bname = args(2)
        val level = args(3).toInt
        val number = args(4).toInt
        val timeout = 900 * 1000
        regrEquivTest(dir, bname, level, number, timeout)

      case "-genBen" =>
        val dir = args(1) //dir to generate benchmarks
        generateBenchmarks(dir)

      case "-genRegrScripts" =>
        val dir = args(1)
        generateRegrScripts(dir)

      case "-analyzeStudentGrammars" =>
        val dirpath = args(1)
        studentdata.StudentDataAnalyzer.analyze(dirpath)

      case "-testVerify" =>
        //testing correctness of equivalence verifier
        //the following is not provable :-( 
        val g1 = bnfgrammar"""S -> a S a | b S b | a | b | "" """
        val g2 = bnfgrammar"""S -> a S a | b S b | a | b | "" """

        implicit val verictx = new EquivalenceVerificationContext(debugEquivVerifier = 1)
        val eqc = new StudentGrammarEquivalenceChecker(g1.cfGrammar)
        eqc.isEquivalentTo(g2.cfGrammar) match {
          case List() => println("no counter-examples")
          case r @ _ =>
            println("found counter-examples: " + r)
            System.exit(0)
        }

        val verifier = new EquivalenceVerifier(g1.cfGrammar, g2.cfGrammar)
        val res = verifier.proveEquivalence()
        if (res.isDefined && res.get)
          println("Equivalence verified")
        else
          println("Cannot prove Equivalence!!")
        gctx.stats.dumpStatsToFile

      case "-evalQuiz" =>
        evalQuiz()
      case "-ra" =>
        generateUsingRAGenerator(OracleJavaGrammar.ebnfGrammar.cfGrammar)
      case "-testEquiv" =>
        val gdb = exercises.GrammarDatabase.readGrammarDatabase(
          new java.io.File("exercises-data/GrammarDatabase.xml"), "exercises-data/")
        val refg = gdb.grammarEntries.find(_.id == 6).get.refGrammar
        val stug = grammar"""S -> a S | a S b | "" """
        val eqc = new StudentGrammarEquivalenceChecker(refg)
        eqc.isEquivalentTo(stug)

      case "-testParsing" =>
        val parser = new AntlrParser(VhdlGrammar1.ebnfGrammar.cfGrammar)
        parser.parse(List(Terminal("IDENTIFIER")))        

      case "-temp" =>
        val g = bnfgrammar"""S -> '(' S ')' S | "" """
        val epg = LLEpslionEliminator.eliminateEpsilons(g.cfGrammar)
        println(epg)
        val gnfg = GNFConverter.toGNF(epg)
        println("GNFG: " + gnfg)
        println("GNFGrammarLL2 ? " + GNFUtilities.isGNFGrammarLL2(gnfg))
      
      case "-testDSL" =>        
        val defaultGrammar = BNFGrammar('S, List(
          'S -> ("a" ~ 'P ~ "b" | ""),
          'P  -> "r" ~ 'S
        ))
        println("DefaultGrammar: "+defaultGrammar)
        println("CFGrammar: "+defaultGrammar.cfGrammar)
        println("isLL1: "+GrammarUtils.isLL1WithFeedback(defaultGrammar.cfGrammar))
        
      case "-testTool" => 
        val toolGrammar = ToolGrammarDSL.ebnfGrammar
        println("DefaultGrammar: "+toolGrammar)
        println("CFGrammar: "+toolGrammar.cfGrammar)
        println("isLL1: "+GrammarUtils.isLL1WithFeedback(toolGrammar.cfGrammar))
        /*implicit val acts = new AmbiguityContext()
        println("isAmbiguous: "+(new AmbiguityChecker(toolGrammar.cfGrammar)).checkAmbiguityInStudentGrammar())*/
        val toolProg = """object IDENTIFIER {
          println ( new IDENTIFIER ( ) . IDENTIFIER ( INTEGER_LITERAL ) ) ;                    
        }
        class IDENTIFIER {
          def IDENTIFIER ( IDENTIFIER : Int ) : Int = {
            var IDENTIFIER : Int ;
            if ( IDENTIFIER < INTEGER_LITERAL )
                    IDENTIFIER = INTEGER_LITERAL ;
                  else
                    IDENTIFIER = IDENTIFIER * ( this . IDENTIFIER ( IDENTIFIER - INTEGER_LITERAL ) ) ;                
            return IDENTIFIER ; 
          }            
        }
        """        
        val tokens =  toolProg.split(" ").map(_.trim()).filterNot { _.isEmpty }.toList
        println("List of tokens: "+tokens.mkString("\n"))
        println("The grammar can parse the string: "+ParseTreeUtils.parse(toolGrammar.cfGrammar, tokens))
        
      case _ =>
        println("Unknown option: " + option)              
    }
    /*opctx.stats.updateCumTime(timer.timeWithoutGC, "TimeWithoutGC")
    opctx.stats.updateCumTime(timer.gcTime, "GCTime")
    opctx.stats.updateCumStats(Stats.peakMemory / 1000, "PeakMemUsageInMB")*/
  }

  def generateRegrScripts(dir: String) {
    genBenchScripts(dir)
    genCFGAScripts(dir)
  }

  def genBenchScripts(dir: String) {
    val grammarsForRegressionCheck = List(C11Grammar1, C11Grammar2, OracleJavaGrammar, AntlrJavaGrammar, JavascriptGrammar1,
      AntlrJavascriptGrammar, SatPaperPascalGrammar, AntlrPascalGrammar, VhdlGrammar1, AntlrVhdlGrammar)
    var scriptStr = ""
    for (level <- 1 to 3) {
      grammarsForRegressionCheck.foreach { b =>
        scriptStr += s"# ${b.benchmarkName} Level: $level" + "\n" //adding a comment
        for (i <- 1 to 10) {
          scriptStr += "sbt \"runMain engine.Main -regr " + s"$dir ${b.benchmarkName} $level $i" + "\"\n"
        }
      }
    }
    //dump the script file
    val pw = new PrintWriter(new FileOutputStream(new File("regr.sh")))
    pw.println(scriptStr)
    pw.close()
  }

  def genCFGAScripts(dir: String) {
    val grammarsForRegressionCheck = List(C11Grammar1, C11Grammar2, AntlrJavaGrammar,
      OracleJavaGrammar, JavascriptGrammar1, AntlrJavascriptGrammar, SatPaperPascalGrammar,
      AntlrPascalGrammar, AntlrVhdlGrammar, VhdlGrammar1)
    var scriptStr = ""
    //val commandPrefix = "/usr/bin/time -f time:%E /home/kandhada/CFG-checking/CFGAnalyzer/bin/cfganalyzer -b 15 -t 900 -q " //note: the timeout is fixed as 900s
    val commandPrefix = "/usr/bin/time -f time:%E ./CFGAnalyzer/bin/cfganalyzer -t 900 -q " //note: the timeout is fixed as 900s
    for (level <- 1 to 3) {
      grammarsForRegressionCheck.foreach { b =>
        scriptStr += s"# ${b.benchmarkName} Level: $level" + "\n" //adding a comment
        //          /"START=$(date +%s)\n"                
        for (i <- 1 to 10) {
          val errgFn = s"${b.benchmarkName}-$level-$i.cfga"
          val errPath = s"$dir/${errgFn}"
          val gpath = s"$dir/${b.benchmarkName}.cfga"
          //add a line to the script file
          scriptStr += s"$commandPrefix $gpath $errPath &> $errgFn-out.txt\n"
        }
        //scriptStr += "END=$(date +%s)\nDIFF=$(( $END - $START ))\necho \"Time: $DIFF seconds\" > " + s"${b.benchmarkName}-cfga-$level.stats" + "\n"
      }
    }
    //dump the script file
    val pw = new PrintWriter(new FileOutputStream(new File("cfga-regr.sh")))
    pw.println(scriptStr)
    pw.close()
  }

  def evalQuiz() {
    implicit val gctx = new GlobalContext()
    implicit val pctx = new ParseContext()
    implicit val ectx = new EnumerationContext()
    implicit val eqctx = new EquivalenceCheckingContext()
    implicit val verictx = new EquivalenceVerificationContext()
    //checkEquivalence(ExpressionsGrammar)
    checkEquivalence(Quiz2010)
    //checkEquivalence(Quiz2009)
    gctx.stats.dumpStatsToFile
    //    evaluateQuiz(IfThenElse)
    //    evaluateQuiz(ThreeMod4)
    //    evaluateQuiz(NotAllAlphabets)                    
  }

  def ambiguityExpts() {
    val ambBenchmarks: List[Benchmark] = List(C11Grammar1, //1
      C11Grammar2, //2      
      OracleJavaGrammar, //3
      AntlrJavaGrammar, //4     
      JavascriptGrammar1, //5     
      AntlrJavascriptGrammar, //6
      SatPaperPascalGrammar, //7
      AntlrPascalGrammar, //8
      VhdlGrammar1, //9
      AntlrVhdlGrammar) //10  

    val timeout = 1800 * 1000
    ambBenchmarks.foreach { bn =>
      //first run GC 
      val rt = Runtime.getRuntime()
      rt.gc()
      Stats.resetPeakMemory

      //create a new operation context, with new log file and stats file
      val gctx = new GlobalContext(statsFilename = bn.benchmarkName + "-amb.stats",
        logFilename = bn.benchmarkName + "-amb.log")
      val ambctx = new AmbiguityContext(consecWordsForAmbiguityCheck = 100000,
        sampledWordsForAmbiguityCheck = 100000, maxSize = 50)
      val enumctx = new EnumerationContext()
      val bnfG = bn.ebnfGrammar
      val g = bnfG.cfGrammar
      //add some statistics
      gctx.stats.updateCumStats(bnfG.rules.size, "#EBNF-rules")
      gctx.stats.updateCumStats(g.rules.size, "#rules")
      gctx.stats.updateCumStats(g.nonTerminals.size, "#nonterminals")

      val ambTimer = new Stats.Timer()
      ambiguityTestDriver(g.fromCNF)(gctx, ambctx, enumctx)

      //add performance statistics
      if (gctx.abort)
        gctx.stats.updateCumStats(timeout, "Timeout") //record that there was a timeout    	  
      gctx.stats.updateCumTime(ambTimer.timeWithoutGC, "TimeWithoutGC")
      gctx.stats.updateCumTime(ambTimer.gcTime, "GCTime")
      gctx.stats.updateCumStats((Stats.peakMemory / 1000), "PeakMemUsageInMB")

      //dump statistics              
      gctx.stats.dumpStatsToFile()
    }
  }

  def ambiguityTestDriver(g: Grammar[String])(implicit gctx: GlobalContext,
    ambctx: AmbiguityContext,
    ectx: EnumerationContext) = {
    val ambChecker = new AmbiguityChecker(g)
    ambChecker.checkForAmbiguity(startSize = 1, fastmode = true)
    /*.foreach {
      _ match {
        case AmbiguityWitnessWithTrees(ant: Nonterminal, w: Word, ptrees) =>
          println("Found ambiguous word: " + wordToString(w))
          //print two parse trees for the word                   
          println("Parse tree 1: " + ParseTreeUtils.parseTreetoString(ptrees(0)))
          println("Parse tree 2: " + ParseTreeUtils.parseTreetoString(ptrees(1)))

        case ambres @ _ =>
          println("Ambigutiy Result: " + ambres)
      }
    }*/
  }

  def equivExpts(dir: String) {
    /*//include this interesting benchmarks in comparison
     * engine.Main.equivTestDriver(grammar.examples.ASequenceOfA.reference.cfGrammar.fromCNF, 
    		grammar.examples.ASequenceOfA.student_grammars(0).grammar.cfGrammar.fromCNF)*/
    val equivBenchmarks: List[Benchmark] = List(C11Grammar1, //1
      C11Grammar2, //2      
      OracleJavaGrammar, //3
      AntlrJavaGrammar, //4     
      JavascriptGrammar1, //5     
      AntlrJavascriptGrammar, //6
      SatPaperPascalGrammar, //7
      AntlrPascalGrammar, //8
      VhdlGrammar1, //9      
      AntlrVhdlGrammarCorrected) //10
    for (i <- 0 until equivBenchmarks.size by 2) {
      //first run GC 
      val rt = Runtime.getRuntime()
      rt.gc()
      Stats.resetPeakMemory

      /*val bn1 = equivBenchmarks(i)
      val bn2 = equivBenchmarks(i + 1)*/
      val b1name = equivBenchmarks(i).benchmarkName
      val b2name = equivBenchmarks(i + 1).benchmarkName

      println(s"Checking equivalence of ${b1name} Vs ${b2name}")

      val fn = b1name + b2name
      //setup parameters
      val gctx = new GlobalContext(statsFilename = fn + "-equiv.stats",
        logFilename = fn + "-equiv.log")
      val eqctx = new EquivalenceCheckingContext(nOfTestcases = 100000,
        timeOut = 60 * 1000, maxSize = 50, continuousMode = true) //timeout of 1 min, in continuous mode
      val enumctx = new EnumerationContext()
      val pctx = new ParseContext()

      /*val g1 = bn1.ebnfGrammar.cfGrammar
      val g2 = bn2.ebnfGrammar.cfGrammar*/
      val g1file = s"${dir}/${b1name}.gram"
      val g2file = s"${dir}/${b2name}.gram"
      val eg1 = GrammarReaders.readFromFile(g1file)
      val eg2 = GrammarReaders.readFromFile(g2file)
      val g1 = eg1.cfGrammar
      val g2 = eg2.cfGrammar
      //add some statistics
      gctx.stats.updateCumStats(eg1.rules.size, "#EBNF-rules1")
      gctx.stats.updateCumStats(eg2.rules.size, "#EBNF-rules2")
      gctx.stats.updateCumStats(g1.rules.size, "#rules1")
      gctx.stats.updateCumStats(g1.nonTerminals.size, "#nonterminals1")
      gctx.stats.updateCumStats(g2.rules.size, "#rules2")
      gctx.stats.updateCumStats(g2.nonTerminals.size, "#nonterminals2")

      val equivTimer = new Stats.Timer()
      equivTestDriver(g1, g2)(gctx, eqctx, enumctx, pctx)

      //add performance statistics
      gctx.stats.updateCumTime(equivTimer.timeWithoutGC, "TimeWithoutGC")
      gctx.stats.updateCumTime(equivTimer.gcTime, "GCTime")
      gctx.stats.updateCumStats(Stats.peakMemory / 1000, "PeakMemUsageInMB")

      //dump statistics              
      gctx.stats.dumpStatsToFile()
    }
  }

  def equivTestDriver(ig1: Grammar[String], ig2: Grammar[String])(implicit gctx: GlobalContext,
    eqctx: EquivalenceCheckingContext,
    ectx: EnumerationContext,
    pctx: ParseContext) {
    val equivChecker = new SamplingBasedEquivalenceChecker(ig1)
    //use continuous mode here 
    equivChecker.isEquivalentTo(ig2)
    //do some cleanup (delete every file in the bin directory)
    cleanup()
  }

  //Takes close to 30min to generate all benchmarks
  def generateBenchmarks(dir: String) {
    generateBenchmark(dir, C11Grammar1, 10)
    generateBenchmark(dir, C11Grammar2, 10)
    generateBenchmark(dir, OracleJavaGrammar, 15)
    generateBenchmark(dir, AntlrJavaGrammar, 15)
    generateBenchmark(dir, SatPaperPascalGrammar, 15)
    generateBenchmark(dir, AntlrPascalGrammar, 15)
    generateBenchmark(dir, VhdlGrammar1, 15)
    generateBenchmark(dir, AntlrVhdlGrammar, 15)
    //difficult to find errors that are deep
    generateBenchmark(dir, JavascriptGrammar1, 10)
    generateBenchmark(dir, AntlrJavascriptGrammar, 7)
  }

  def generateBenchmark(dir: String, b: Benchmark, minErrorLength: Int) = {
    val g = b.ebnfGrammar.cfGrammar
    val prefix = s"$dir/${b.benchmarkName}"

    val gctx = new GlobalContext(logFilename = s"$prefix-Gen.log")
    val nOfErrors = 10
    val seed = java.util.Calendar.getInstance().getTimeInMillis()
    val errorGen = new RandomErrorGeneration(g, seed)
    gctx.logMessage("Seed: " + seed) //log the seed for replaying

    GrammarWriter.dumpPrettyGrammar(prefix, g)
    GrammarToCFGA.grammarToCFGA(g, s"$prefix.cfga")

    for (level <- 1 to 3) {
      for (i <- 1 to nOfErrors) {
        val (errg, errLen) = errorGen.createErrors(level, minErrorLength, 50)
        //dump errg to a file
        val errfile = s"$prefix-$level-$i"
        GrammarWriter.dumpPrettyGrammar(errfile, errg)
        GrammarToCFGA.grammarToCFGA(errg, s"$errfile.cfga")

        val msg = s"Bechmark: ${b.benchmarkName} Level:$level number:$i - Error length: $errLen"
        gctx.logMessage(msg)
        println(msg)
      }
    }
  }

  def regrEquivTest(dir: String, bname: String, level: Int, number: Int, timeout: Long) = {
    val g1file = s"${dir}/${bname}.gram"
    val g2file = s"${dir}/${bname}-$level-$number.gram"
    val b1 = GrammarReaders.readFromFile(g1file)
    val b2 = GrammarReaders.readFromFile(g2file)
    println(s"Checking Level $level Number $number of ${bname}")

    val fn = s"${bname}-$level-$number"
    //this defines all the parameters used by the operations
    implicit val gctx = new GlobalContext(statsFilename = fn + ".stats", logFilename = fn + ".log")
    implicit val eqctx = new EquivalenceCheckingContext(nOfTestcases = 100000, maxSize = 50, timeOut = timeout)
    implicit val enumctx = new EnumerationContext(maxIndexSizeForGeneration = 22) //note: use at most 22 bit indices
    implicit val pctx = new ParseContext()

    gctx.stats.updateCumStats(b1.rules.size, "#EBNF-rules")
    gctx.stats.updateCumStats(b1.cfGrammar.rules.size, "#rules")
    gctx.stats.updateCumStats(b1.cfGrammar.nonTerminals.size, "#nonterminals")
    val equivTimer = new Stats.Timer()
    val equivChecker = new SamplingBasedEquivalenceChecker(b1.cfGrammar)

    equivChecker.isEquivalentTo(b2.cfGrammar)

    //add performance statistics
    gctx.stats.updateCumTime(equivTimer.timeWithoutGC, "TimeWithoutGC")
    gctx.stats.updateCumTime(equivTimer.gcTime, "GCTime")
    gctx.stats.updateCumStats(Stats.peakMemory / 1000, "PeakMemUsageInMB")
    //dump statistics              
    gctx.stats.dumpStatsToFile()
    cleanup()
  }

  def cleanup() {
    val bindir = new File("./bin")
    bindir.listFiles().foreach(_.delete())
  }
}
  
