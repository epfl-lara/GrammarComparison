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
import grammar.GrammarUtils

object EvaluateQuiz {

  def checkEquivalence(quiz: QuizResult)(implicit gctx: GlobalContext,
    pctx: ParseContext,
    ectx: EnumerationContext,
    eqctx: EquivalenceCheckingContext,
    verictx: EquivalenceVerificationContext) {
    
    def proveEquivalence(g1: Grammar[String], g2: Grammar[String]) = {
      println("Trying to prove equivalence...")
      val verifier = new EquivalenceVerifier(g1, g2)
      verifier.proveEquivalence() match {
        case Some(true) =>
          println("The grammars are proved to be equivalent!")
        case _ =>
          println("Cannot prove equivalence!")
      }
    }

    val ref = quiz.reference.cfGrammar
    val equivChecker = new StudentGrammarEquivalenceChecker(ref)

    var i = 1
    println("Reference Grammar: ")
    println(quiz.reference)
    println("===============")
    quiz.student_grammars.foreach(studentGrammar => {
      println("Evaluating solution " + i)
      println(studentGrammar.grammar)

      val g = studentGrammar.grammar.cfGrammar
      if (g.cnfGrammar.rules.isEmpty) {
        println("The grammar is empty. Not all rules are produtive and reachable !!")
      } else {
        //println("Student Grammar In GNF: " + GNFConverter.toGNF(cnfG))
        //proveEquivalence(equivChecker.cnfRef, cnfG)
        equivChecker.isEquivalentTo(g) match {
          case List() =>
            println(s"Possibly Equivalent")
            proveEquivalence(ref, g)
          case l @ _ =>
            l.foreach {
              case equivResult @ NotEquivalentNotAcceptedBySolution(_) =>
                println(s"System:  Wrong. $equivResult")
              case equivResult @ NotEquivalentGeneratedBySolution(_) =>
                println(s"System:  Wrong. $equivResult")
              case eres @ _ =>
                throw new IllegalStateException("Did not expect: " + eres)
            }
        }
      }
      println("LL1:     " + GrammarUtils.isLL1WithFeedback(g))
      println("===============")
      i += 1
    })
    //for stats    
    val pr = new java.io.PrintWriter(quiz.quizName + "-stats.txt")
    gctx.stats.dumpStats(pr.println)
    pr.close()    
  }
  /*val resG = repairer.repair(cnfG, equivResult)            
            println("Final grammar: "+ prettyPrint(resG))*/
  //proveEquivalence(equivChecker.cnfRef, resG)  
}