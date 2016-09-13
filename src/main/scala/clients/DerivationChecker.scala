package clients
import grammar._
import utils._
import CFGrammar._

object DerivationChecker {

  type Word = List[Terminal]
  type SententialForm = List[Symbol]

  sealed abstract class DerivationFeedback
  case class Correct() extends DerivationFeedback
  case class InvalidStart() extends DerivationFeedback
  case class InvalidEnd() extends DerivationFeedback
  case class WrongStep(from: SententialForm, to: SententialForm, msg: String) extends DerivationFeedback
  case class Other(msg: String) extends DerivationFeedback
  
  def checkLeftMostDerivation[T](word: List[Terminal], steps: List[SententialForm], g: Grammar[T]): DerivationFeedback = {
    if (steps.isEmpty) {
      Other("No derivation is given")
    } else {
      val start = steps.head      
      if (start != List(g.start)) {
        InvalidStart()
      } else {
        //find a step that violates the derivation	      
        val (fb, last) = steps.tail.foldLeft((None: Option[DerivationFeedback], start)) {
          case ((None, prev), step) =>
            //pick the first nonterminal of 'prev'
            val firstntIndex = prev.indexWhere(_.isInstanceOf[Nonterminal])
            if (firstntIndex < 0) {
              //check if 'prev == step' 
              if (prev == step) (None, step)
              else
                (Some(WrongStep(prev, step,
                  "The sentential forms need to be identical as there are no nonterminals to expand")), step)
            } else {
              //check if the 'fistntIndex' terminals agree
              if (prev.take(firstntIndex) != step.take(firstntIndex)) {
                (Some(WrongStep(prev, step,
                  "The prefix terminals are not identical")), step)
              } else {   
                val prevSuff = prev.drop(firstntIndex)
                val nt = prevSuff.head.asInstanceOf[Nonterminal]                                
                val stepSuff = step.drop(firstntIndex)
                if (rightSides(nt, g).exists(rside =>
                  (rside ++ prevSuff.tail) == stepSuff)) {
                  (None, step)
                } else {
                  (Some(WrongStep(prev, step,
                    "No production of " + nt + " agrees with the derivation")), step)
                }
              }
            }
          case (acc, _) => acc
        }
        if (fb.isDefined)
          fb.get
        else {
          if (last == word) Correct()
          else InvalidEnd()
        }
      }
    }
  }
}