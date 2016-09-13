package equivalence
import grammar._
import repair._
import parsing._
import generators._
import CFGrammar._
import EBNFGrammar._
import BNFConverter._
import grammar.utils._

/**
 * As of now, this can be used only with grammars whose terminals are strings as it depends on AntlrParser.
 */
class StudentGrammarEquivalenceChecker(g1: Grammar[String])(implicit gctx: GlobalContext,
  opctx: EquivalenceCheckingContext,
  enumctx: EnumerationContext,
  parsectx: ParseContext) extends EquivalenceChecker[String] {

  val refg = CFGrammar.appendSuffix("1", g1)
  //using the grammar without epsilon and unit productions for enumeration
  val refGen = new SizeBasedRandomAccessGenerator(refg.fromCNF, opctx.maxSize)
  val refParser =
    if (opctx.useAntlrForStudentGrammars)
      new AntlrParserInterpreted(refg)
    else refCYK
  //using the CNF grammar for CYK parsing
  lazy val refCYK = new CYKParser(refg.cnfGrammar)

  /**
   * Increments size in steps and looks for counter-example of each size
   */
  def isEquivalentTo(g2: Grammar[String]) = {

    //for stats
    gctx.stats.updateCounter(1, "EqTestCalls")
    val statsTimer = new Stats.Timer()

    //create a thread that sets stop flag in 'timeout' ms
    var stop = false
    val task = Util.scheduleTask(() => { stop = true }, opctx.timeOut)

    val g = CFGrammar.appendSuffix("2", g2)
    //for every size from 1 to 'maxWordSize' check if every word 
    //in one grammar is contained in the other
    val startSize = opctx.startSize
    val maxSize = opctx.maxSize
    val gGen = new SizeBasedRandomAccessGenerator(g.fromCNF, maxSize)
    val nos = opctx.nOfTestcases
    var ctrExamples = List[EquivalenceResult]()

    val gCYK = new CYKParser(g.cnfGrammar)
    val gparser = if (opctx.useAntlrForStudentGrammars) new AntlrParserInterpreted(g)
    else gCYK

    var break = false
    for (size <- startSize to maxSize) if (!break && !stop) {

      val (giter, riter) =
        {
          val gwords = gGen.getSeqEnumerator(g.fromCNF.start, size, nos).toSet
          val rwords = refGen.getSeqEnumerator(refg.fromCNF.start, size, nos).toSet
          val gminusr = gwords -- rwords
          val rminusg = rwords -- gwords
          (gminusr.iterator, rminusg.iterator)
        }

      while (!break && (riter.hasNext || giter.hasNext) && !stop) {
        if (riter.hasNext) {
          val x = riter.next
          if (opctx.debugEquivChecker > 1) {
            println("Trying to parse string: " + wordToString(x))
          }

          if (!gparser.parse(x)) {
            val eqres = NotEquivalentNotAcceptedBySolution(x)
            /*val msg = "Found counter-example of size " + size + " : " + eqres
            println(msg)
            opctx.logMessage(msg)*/

            if (opctx.useAntlrForStudentGrammars) {
              gctx.stats.updateCumStats(1, "AntlrEquivCExs")
              //here cross check using CYK parser  
              if (!gCYK.parse(x)) {
                val msg = "Counter-example Verified!"
                println(msg)
                gctx.logMessage(msg)

                ctrExamples :+= eqres
                //update the statistics
                gctx.stats.updateCumStats(1, "EquivCExs")
                //there is a ref tree but not a g tree          
                /*opctx.logMessage("Ref Parse Tree: \n"+ 
                ParseTreeUtils.parseTreetoString(refParser.parseWithTree(x).get)) //should we use index here instead ?*/
                break = true;
              } else {
                val msg = "Counter-example Refuted!"
                println(msg)
                gctx.logMessage(msg)
                gctx.stats.updateCumStats(1, "AntlrEquivFalsePos")
              }
            } else {
              ctrExamples :+= eqres
              //update the statistics
              gctx.stats.updateCumStats(1, "EquivCExs")
              break = true;
            }
          }
        }
        if (giter.hasNext && !break && !stop) {
          val y = giter.next
          if (opctx.debugEquivChecker > 1) {
            println("Trying to validate string: " + wordToString(y))
          }

          if (!refParser.parse(y)) {
            //there exists a word that should not be accepted
            val eqres = NotEquivalentGeneratedBySolution(y)
            /*val msg = "Found counter-example of size " + size + " : " + eqres
            println(msg)
            opctx.logMessage(msg)*/
            if (opctx.useAntlrForStudentGrammars) {
              gctx.stats.updateCumStats(1, "AntlrEquivCExs")
              //cross check the counter-example            
              if (!refCYK.parse(y)) {
                val msg = "Counter-example Verified!"
                println(msg)
                gctx.logMessage(msg)

                ctrExamples :+= eqres
                //update the statistics
                gctx.stats.updateCumStats(1, "EquivCExs")
                //there is a g tree but not a ref tree
                /*opctx.logMessage("G Parse Tree: \n"+
                ParseTreeUtils.parseTreetoString(gParser.parseWithTree(y).get)) //should we use index here instead ?*/
                break = true;
              } else {
                val msg = "Counter-example Refuted!"
                println(msg)
                gctx.logMessage(msg)
                gctx.stats.updateCumStats(1, "AntlrEquivFalsePos")
              }
            } else {
              ctrExamples :+= eqres
              gctx.stats.updateCumStats(1, "EquivCExs")
              break = true;
            }
          }
        }
      }
    }
    if (stop) {
      val msg = "Timeout while checking equivalence!"
      //println(msg)
      gctx.logMessage(msg)
    } else {
      if (task.isDefined)
        task.get.cancel()
      if (ctrExamples.isEmpty) {
        val msg = "Possibly equivalent!"
        gctx.logMessage(msg)
        //        /println(msg)
      }
    }
    //for stats
    gctx.stats.updateCounterTime(statsTimer.timeWithoutGC, "EqTestTime", "EqTestCalls")
    ctrExamples
  }

  //is g2 subset ref ?
  def isSubset(g2: Grammar[String]) = {
    //create a thread that sets stop flag in 'timeout' ms
    var stop = false
    val task = Util.scheduleTask(() => { stop = true }, opctx.timeOut)

    val g = CFGrammar.appendSuffix("2", g2)
    //for every size from 1 to 'maxWordSize' check if every word 
    //in g2 is contained in ref
    val startSize = opctx.startSize
    val maxSize = opctx.maxSize
    val gGen = new SizeBasedRandomAccessGenerator(g.fromCNF, maxSize)
    val nos = opctx.nOfTestcases
    var ctrExamples = List[Word]()

    var break = false
    for (size <- startSize to maxSize) if (!break && !stop) {

      val gwords = gGen.getSeqEnumerator(g.fromCNF.start, size, nos).toSet
      val rwords = refGen.getSeqEnumerator(refg.fromCNF.start, size, nos).toSet
      val gminusr = gwords -- rwords
      val giter = gminusr.iterator
      while (!break && giter.hasNext && !stop) {
        val y = giter.next
        if (opctx.debugSubsetCheck > 1) {
          println("Trying to validate string: " + wordToString(y))
        }
        if (!refParser.parse(y)) {
          /*val msg = "Found counter-example of size " + size + " : " + eqres
            println(msg)
            opctx.logMessage(msg)*/
          if (opctx.useAntlrForStudentGrammars) {
            gctx.stats.updateCumStats(1, "AntlrEquivCExs")
            //cross check the counter-example            
            if (!refCYK.parse(y)) {
              val msg = "Counter-example Verified!"
              println(msg)
              gctx.logMessage(msg)
              ctrExamples :+= y
              //there is a g tree but not a ref tree
              /*opctx.logMessage("G Parse Tree: \n"+
                ParseTreeUtils.parseTreetoString(gParser.parseWithTree(y).get)) //should we use index here instead ?*/
              break = true;
            } else {
              val msg = "Counter-example Refuted!"
              println(msg)
              gctx.logMessage(msg)
              gctx.stats.updateCumStats(1, "AntlrEquivFalsePos")
            }
          } else {
            ctrExamples :+= y
            break = true;
          }
        }
      }
    }
    if (stop) {
      val msg = "Timeout while checking subset!"
      //println(msg)
      gctx.logMessage(msg)
    } else {
      if (task.isDefined)
        task.get.cancel()
      if (ctrExamples.isEmpty) {
        val msg = "Possibly Subset!"
        gctx.logMessage(msg)
        //        /println(msg)
      }
    }
    //for stats    
    ctrExamples
  }

  def isSuperset(g2: Grammar[String]) = {
    //create a thread that sets stop flag in 'timeout' ms
    var stop = false
    val task = Util.scheduleTask(() => { stop = true }, opctx.timeOut)

    val g = CFGrammar.appendSuffix("2", g2)
    //for every size from 1 to 'maxWordSize' check if every word 
    //in g2 is contained in ref
    val startSize = opctx.startSize
    val maxSize = opctx.maxSize
    val nos = opctx.nOfTestcases
    val gGen = new SizeBasedRandomAccessGenerator(g.fromCNF, maxSize)
    val gCYK = new CYKParser(g.cnfGrammar)
    val gparser = if (opctx.useAntlrForStudentGrammars) new AntlrParserInterpreted(g)
    else gCYK

    var ctrExamples = List[Word]()
    var break = false
    for (size <- startSize to maxSize) if (!break && !stop) {

      val rwords = refGen.getSeqEnumerator(refg.fromCNF.start, size, nos).toSet
      val gwords = gGen.getSeqEnumerator(g.fromCNF.start, size, nos).toSet
      val rminusg = rwords -- gwords
      val riter = rminusg.iterator

      while (!break && riter.hasNext && !stop) {
        val x = riter.next
        if (opctx.debugSupersetCheck > 1) {
          println("Trying to parse string: " + wordToString(x))
        }

        if (!gparser.parse(x)) {
          /*val msg = "Found counter-example of size " + size + " : " + eqres
            println(msg)
            opctx.logMessage(msg)*/

          if (opctx.useAntlrForStudentGrammars) {
            gctx.stats.updateCumStats(1, "AntlrEquivCExs")
            //here cross check using CYK parser  
            if (!gCYK.parse(x)) {
              val msg = "Counter-example Verified!"
              println(msg)
              gctx.logMessage(msg)

              ctrExamples :+= x
              //update the statistics
              gctx.stats.updateCumStats(1, "EquivCExs")
              //there is a ref tree but not a g tree          
              /*opctx.logMessage("Ref Parse Tree: \n"+ 
                ParseTreeUtils.parseTreetoString(refParser.parseWithTree(x).get)) //should we use index here instead ?*/
              break = true;
            } else {
              val msg = "Counter-example Refuted!"
              println(msg)
              gctx.logMessage(msg)
              gctx.stats.updateCumStats(1, "AntlrEquivFalsePos")
            }
          } else {
            ctrExamples :+= x
            //update the statistics
            gctx.stats.updateCumStats(1, "EquivCExs")
            break = true;
          }
        }
      }
    }
    if (stop) {
      val msg = "Timeout while checking subset!"
      //println(msg)
      gctx.logMessage(msg)
    } else {
      if (task.isDefined)
        task.get.cancel()
      if (ctrExamples.isEmpty) {
        val msg = "Possibly Superset!"
        gctx.logMessage(msg)
        //        /println(msg)
      }
    }
    //for stats    
    ctrExamples
  }
  
  def getRefWords = {
    val startSize = opctx.startSize
    val maxSize = opctx.maxSize  
    val nos = opctx.nOfTestcases
    (startSize to maxSize).flatMap { size => 
      refGen.getSeqEnumerator(refg.fromCNF.start, size, nos)
    }.toList.distinct
  }
}