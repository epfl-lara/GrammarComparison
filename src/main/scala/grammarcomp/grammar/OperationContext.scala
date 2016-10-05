package grammarcomp

package grammar

import java.io._
/**
 * A set of parameters to the system.
 * Static parameters cannot be changed after an operation starts executing,
 * Dynamic parameters can be changed after the operation has started executing.
 * Basically, this serves as a knob that lets the user configure the system and also
 * abort operations after they have been issued
 */
class GlobalContext(
  //debug flags of some generic operations
  val debugGNFConversion : Boolean = false,
  //stats and logs
  val enableStats: Boolean = false,
  val enableLog: Boolean = false,
  val statsFilename: String = "cfg-checker.stats",
  val logFilename: String = "cfgchecker.log") {

  //enter static parameters here
  val stats = new grammar.utils.Stats(statsFilename)
  val logStream = 
    if (enableLog) 
      new PrintWriter(new FileOutputStream(new File(logFilename)))
    else null

  //enter any dynamic parameters here
  var abort = false

  def logMessage(msg: String) = if (enableLog) {
    logStream.println(msg)
    logStream.flush()
  }

  override def finalize() {
    logStream.close()
  }
}

class EnumerationContext(
  val debugGenerator: Int = 0,
  val verifyInverseCantor: Boolean = false,
  val maxIndexSizeToCache: Int = 10,  
  val maxIndexSizeForGeneration: Int = Int.MaxValue //no limits on the size of the index, by default  
  ) {
	
}

class EquivalenceCheckingContext( //debug levels for equivalence checker
  val debugEquivChecker: Int = 0,
  val debugSubsetCheck : Int = 0,
  val debugSupersetCheck : Int = 0,
  val useAntlrForStudentGrammars: Boolean = false,
  //make sure that number is testcases is much less than the 'maxIndexForGeneration'
  val nOfTestcases: Int = 100,
  val startSize: Int = 1,
  val maxSize: Int = 11, // used by equivalence checker to limit the sizes of the words
  val timeOut: Long = -1,
  val continuousMode: Boolean = false) {

}

class AmbiguityContext(val debugAmbiguityCheck: Boolean = false,
  val maxSize: Int = 50, // used to limit the sizes of the words
  val consecWordsForAmbiguityCheck: Int = 300,
  val sampledWordsForAmbiguityCheck: Int = 300) {

}

class ParseContext(val debugAntlrInterpreter: Boolean = false,
  val antlrCompilationDir: String = "./bin/",
  val antlrJarPath: String = "./lib/antlr-4.5-complete.jar") {
}

class EquivalenceVerificationContext(
  val printVerifcationFeedback: Boolean = true,
  val debugEquivVerifier: Int = 0,
  val debugTestcasesSimplification: Boolean = false,
  val debugSententialFormParsing: Boolean = false,
  val debugBtrans: Boolean = false,
  val debugGNFConversion: Boolean = false,
  val verificationTimeout: Int = 10, //in seconds
  val useTestcasesInVerification: Boolean = true,
  val testsForVerification: Int = 100,
  val maxSizeForVerification: Int = 11) {

}

class RepairContext( //debug flags for repair
  val debugSupersetRepair: Boolean = false,
  val debugSubsetRepair: Boolean = false,
  val debugSubsetIteration: Boolean = false,
  val debugSupersetIteration: Boolean = false,
  val debugRepair: Int = 0,
  //number of correct words (or) parse trees based on which we must do repair
  //this can be used to adjust the precision and scalability of (superset) repair
  //Note: we can be a little imprecise in generating hints
  val nCorrectWordsForRepair: Int = 100,
  val enableExpensiveRepair: Boolean = false) {

}
