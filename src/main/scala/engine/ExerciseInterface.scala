/*package engine

import grammar.examples._
import grammar.utils._
import grammar._
import repair._
import parsing._
import CFGrammar._
import EBNFGrammar._
import BNFConverter._
import equivalence._
import generators.LazyGenerator
import grammar.exercises._
import grammar.exercises._
import grammar.GrammarUtils

object ExerciseInterface {

  val debug = false   

  def postExerciseSet(exset: GrammarDatabase)(implicit opctx : OperationContext) {
    println("Enter exercise id: ")
    val exid = scala.io.StdIn.readInt()
    postExercise(exset.grammarEntries.find(_.id == exid).get)
  }

  def postExercise(ex: GrammarEntry)(implicit opctx : OperationContext) {
	      
    println("===============")
    println(ex.id + ": " + ex.name)
    println(ex.desc)
    
    var loop = true
    while (loop) {
      //read students grammar
      val studentGrammar = GrammarReaders.readFromConsole
      println("Evaluating solution ")
      
      val equivChecker = new EquivalenceChecker(ex.cnfRef)      
      val repairer = new Repairer(equivChecker)

      println("Reference Grammar: ")
    println(ex.reference)

      println("Reference Grammar In CNF: " + equivChecker.cnfRef)
      val plainGrammar = ebnfToGrammar(studentGrammar)
      val cnfG = plainGrammar.cnfGrammar
      println("Your Grammar after normalization and epsilon removal: " + plainGrammar.fromCNF)      

      if (cnfG.rules.isEmpty) {
        println("The grammar is empty. Not all rules are produtive and reachable !!")
      } else {

        println("Student Grammar In CNF: " + cnfG)
        //println("Student Grammar In GNF: " + GNFConverter.toGNF(cnfG))
        //proveEquivalence(equivChecker.cnfRef, cnfG)

        equivChecker.isEquivalentTo(cnfG) match {
          case equivResult @ PossiblyEquivalent => {
            println(s"System:  $equivResult")
            proveEquivalence(equivChecker.cnfRef, cnfG)
          }
          case equivResult @ (InadequateTestcases | Aborted) => {
            println(s"System:  $equivResult")
          }
          case equivResult @ NotEquivalentNotAcceptedBySolution(_) =>
            println(s"System:  Wrong. $equivResult")
            //the following is for testing
            //proveEquivalence(equivChecker.cnfRef, CNFConverter.toCNF(cnfG))
            
            val (resG, feedbacks) = repairer.hint(cnfG, equivResult)
            //println("resG: "+resG)
            feedbacks.foreach(f => println(f.toString))            
            //proveEquivalence(equivChecker.cnfRef, CNFConverter.toCNF(resG))
            
          case equivResult @ NotEquivalentGeneratedBySolution(_) =>
            println(s"System:  Wrong. $equivResult")
            //the following is for testing
            //proveEquivalence(equivChecker.cnfRef, CNFConverter.toCNF(cnfG))

            val (resG, feedbacks) = repairer.hint(cnfG, equivResult)
            //println("resG: "+resG)
            feedbacks.foreach(f => println(f.toString))
            //proveEquivalence(equivChecker.cnfRef, CNFConverter.toCNF(resG))
        }
        println("LL1:     " + GrammarUtils.isLL1WithFeedback(plainGrammar))
      }
      println("===============")
      println("Retry ? (y/n)")
      if (scala.io.StdIn.readChar() != 'y')
        loop = false
    }
    //for stats    
    val pr = new java.io.PrintWriter(quiz.quizName + "-stats.txt")
    Stats.dumpStats(pr)
    pr.close()
  }

  def proveEquivalence(g1: Grammar, g2: Grammar)(implicit opctx : OperationContext) = {
    println("Trying to prove equivalence...")
    val verifier = new EquivalenceVerifier(g1, g2) //we can be a little imprecise here in generating the number of tests
    verifier.proveEquivalence() match {
      case Some(true) =>
        println("The grammars are proved to be equivalent!")
      case _ =>
        println("Cannot prove equivalence!")
    }
  }
}*/