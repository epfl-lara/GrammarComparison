package equivalence
import grammar._
import repair._
import parsing._
import generators._
import CFGrammar._
import EBNFGrammar._
import BNFConverter._
import utils._
import java.util.concurrent.Executors

/**
 * Takes a reference grammar in any form.
 * This uses Antlr parser and hence can work only with Grammars whose terminals are strings.
 */
class SamplingBasedEquivalenceChecker(g1: Grammar[String])(implicit gctx: GlobalContext,
  opctx: EquivalenceCheckingContext,
  enumctx: EnumerationContext,
  parsectx: ParseContext) extends EquivalenceChecker[String] {

  val refg = CFGrammar.appendSuffix("1", g1)
  //using the grammar without epsilon and unit productions for enumeration
  lazy val refGen = new SizeBasedRandomAccessGenerator(refg.fromCNF, opctx.maxSize)
  //using the plain grammar for AntlrParsing
  lazy val refParser = new AntlrParser(refg)
  //using the CNF grammar for CYK parsing
  lazy val refCYK = new CYKParser(refg.cnfGrammar)

  /**
   * Checks for equivalence between the given grammar and the 'ref' grammar
   */
  def isEquivalentTo(g2: Grammar[String]) = {

    //create a thread that sets stop flag in 'timeout' ms
    var stop = false
    val task = Util.scheduleTask(() => { stop = true }, opctx.timeOut)

    val g = CFGrammar.appendSuffix("2", g2)
    //for every size from 1 to 'maxWordSize' check if every word 
    //in one grammar is contained in the other
    val startSize = opctx.startSize
    val maxSize = opctx.maxSize
    val nos = opctx.nOfTestcases

    //using the grammar without epsilons and unit productions for enumeration    
    val gEG = g.fromCNF
    val gGen = new SizeBasedRandomAccessGenerator(gEG, maxSize)
    //using the plain grammar for AntlrParsing    
    val gParser = new AntlrParser(g)
    //using the CNF grammar for CYK parsing    
    val gCYK = new CYKParser(g.cnfGrammar)

    //compute the smallest size at which the parse tree counts  diverge
    var break = false
    for (size <- startSize to maxSize) if (!break) {
      //println("Checking counter-examples of size: " + size)
      val refDomainSize = refGen.wordCounter.boundForNonterminal(refg.start, size)
      val gDomainSize = gGen.wordCounter.boundForNonterminal(g.start, size)

      if (refDomainSize != gDomainSize) {
        val msg = "Number of parse trees of size " + size + " do not agree (#(g1 trees) - #(g2 trees)) = " + (refDomainSize - gDomainSize) +
          " max. number of trees : " + (if (refDomainSize > gDomainSize) refDomainSize else gDomainSize)
        //println(msg)
        gctx.logMessage(msg)
        break = true
      }
    }

    //construct a bunch of sampling enumerators for each size           
    val refEnums = (startSize to maxSize).map { sz => refGen.getSamplingEnumerator(refg.fromCNF.start, sz, nos) }.toArray
    val gEnums = (startSize to maxSize).map { sz => gGen.getSamplingEnumerator(gEG.start, sz, nos) }.toArray

    var ctrExamples = List[EquivalenceResult]()
    var toExploreSizes = (startSize to maxSize).toList
    val rand = new java.util.Random()
    break = false
    while (!break && !toExploreSizes.isEmpty && !stop) {
      val exploreIndex = rand.nextInt(toExploreSizes.size) //randomly sample a size
      val size = toExploreSizes(exploreIndex)

      //for debugging      
      //println("Accessing a random word of size: " + size)

      val refEnum = refEnums(size - 1)
      val gEnum = gEnums(size - 1)
      var foundCtex = false
      if (refEnum.hasNext || gEnum.hasNext) {
        if (refEnum.hasNext) {
          val x = refEnum.next
          if (opctx.debugEquivChecker > 1) {
            println("Trying to parse string: " + wordToString(x))
          }

          if (!gParser.parse(x)) {
            val eqres = NotEquivalentNotAcceptedBySolution(x)
            val msg = "Found counter-example of size " + size + " : " + eqres
            println(msg)
            gctx.logMessage(msg)
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
                ParseTreeUtils.parseTreetoString(refParser.parseWithTree(x).get)) //should we use index here instead ?
               */
              foundCtex = true;
            } else {
              val msg = "Counter-example Refuted!"
              println(msg)
              gctx.logMessage(msg)
              gctx.stats.updateCumStats(1, "AntlrEquivFalsePos")
            }
          }
        }
        if (gEnum.hasNext && !foundCtex) {
          val y = gEnum.next
          if (opctx.debugEquivChecker > 1) {
            println("Trying to validate string: " + wordToString(y))
          }

          if (!refParser.parse(y)) {
            //there exists a word that should not be accepted
            val eqres = NotEquivalentGeneratedBySolution(y)
            val msg = "Found counter-example of size " + size + " : " + eqres
            println(msg)
            gctx.logMessage(msg)
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
                ParseTreeUtils.parseTreetoString(gParser.parseWithTree(y).get)) //should we use index here instead ?
               */
              foundCtex = true;
            } else {
              val msg = "Counter-example Refuted!"
              println(msg)
              gctx.logMessage(msg)
              gctx.stats.updateCumStats(1, "AntlrEquivFalsePos")
            }
          }
        }
        if (foundCtex && !opctx.continuousMode) {
          //break here
          break = true
          //remove this size as we found a counter-example for this size
          //toExploreSizes = toExploreSizes.filterNot(_ == size)
        }
      } else
        toExploreSizes = toExploreSizes.filterNot(_ == size) //every word of this size has been explored in both the grammars      
    }
    if (stop) {
      val msg = "timeout!"
      println(msg)
      gctx.logMessage(msg)
    } else {
      if (task.isDefined) //cancel the timer if there was one
        task.get.cancel()
      if (ctrExamples.isEmpty) {
        val msg = "Possibly equivalent!"
        gctx.logMessage(msg)
        println(msg)
      }
    }
    ctrExamples
  }

  def isSubset(g2: Grammar[String]) = {
    //create a thread that sets stop flag in 'timeout' ms
    var stop = false
    val task = Util.scheduleTask(() => { stop = true }, opctx.timeOut)

    val g = CFGrammar.appendSuffix("2", g2)
    //for every size from startSize to 'maxWordSize' check if every word 
    //in g2 is contained in refg
    val startSize = opctx.startSize
    val maxSize = opctx.maxSize
    val nos = opctx.nOfTestcases

    //using the grammar without epsilons and unit productions for enumeration    
    val gEG = g.fromCNF
    val gGen = new SizeBasedRandomAccessGenerator(gEG, maxSize)

    //construct a bunch of sampling enumerators for each size              
    val gEnums = (startSize to maxSize).map { sz => gGen.getSamplingEnumerator(gEG.start, sz, nos) }.toArray

    var ctrExamples = List[Word]()
    var toExploreSizes = (startSize to maxSize).toList
    val rand = new java.util.Random()
    var break = false
    while (!break && !toExploreSizes.isEmpty && !stop) {
      val exploreIndex = rand.nextInt(toExploreSizes.size) //randomly sample a size
      val size = toExploreSizes(exploreIndex)
      val gEnum = gEnums(size - 1)
      var foundCtex = false
      if (gEnum.hasNext) {
        val y = gEnum.next
        if (!refParser.parse(y)) {
          //there exists a word that should not be accepted
          val msg = "Found counter-example of size " + size + " : " + y
          println(msg)
          gctx.logMessage(msg)
          gctx.stats.updateCumStats(1, "AntlrEquivCExs")

          //cross check the counter-example
          if (!refCYK.parse(y)) {
            val msg = "Counter-example Verified!"
            println(msg)
            gctx.logMessage(msg)

            ctrExamples :+= y
            //update the statistics
            gctx.stats.updateCumStats(1, "EquivCExs")
            foundCtex = true;
          } else {
            val msg = "Counter-example Refuted!"
            println(msg)
            gctx.logMessage(msg)
            gctx.stats.updateCumStats(1, "AntlrEquivFalsePos")
          }
        }
      } else
        toExploreSizes = toExploreSizes.filterNot(_ == size) //every word of this size has been explored 
      if (foundCtex && !opctx.continuousMode) {
        //break here
        break = true
      }
    }
    if (stop) {
      val msg = "timeout!"
      println(msg)
      gctx.logMessage(msg)
    } else {
      if (task.isDefined) //cancel the timer if there was one
        task.get.cancel()
      if (ctrExamples.isEmpty) {
        val msg = "Possibly equivalent!"
        gctx.logMessage(msg)
        println(msg)
      }
    }
    ctrExamples
  }

  def isSuperset(g2: Grammar[String]) = {
    throw new IllegalStateException("Not implemented yet!")
  }

  def getRefWords = {
    throw new IllegalStateException("Not implemented yet!")
  }

  /**
   * Checks if the predicate holds for strings sampled at random
   */
  def checkPredicateHolds(pred: Word => Boolean): List[Word] = {

    var stop = false
    val task = Util.scheduleTask(() => { stop = true }, opctx.timeOut)
    //generators for g          
    val startSZ = opctx.startSize
    val maxSZ = opctx.maxSize
    val nos = opctx.nOfTestcases
    val refEnums = (startSZ to maxSZ).map { sz => refGen.getSamplingEnumerator(refg.fromCNF.start, sz, nos) }.toArray
    var toExploreSizes = (startSZ to maxSZ).toList
    val rand = new java.util.Random()
    var break = false
    var ctrex = List[Word]()
    while (!break && !toExploreSizes.isEmpty && !stop && !gctx.abort) {
      val exploreIndex = rand.nextInt(toExploreSizes.size) //randomly sample a size
      val size = toExploreSizes(exploreIndex)
      val refEnum = refEnums(size - 1)
      if (refEnum.hasNext) {
        val y = refEnum.next
        if (!pred(y)) {
          ctrex :+= y
          break = true;
        }
      } else
        toExploreSizes = toExploreSizes.filterNot(_ == size)
    }
    if (task.isDefined) //cancel the timer if there was one
      task.get.cancel()
    ctrex  
  }
}