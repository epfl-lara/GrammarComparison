package clients
import grammar._
import utils._
import CFGrammar._
import generators._
import generators.RandomAccessGenerator._
import parsing._
import scala.collection.mutable.{ Map => MutableMap }
import java.util.Random

object AmbiguityChecker {

  sealed abstract class AmbiguityFeedback
  case class PossiblyUnambiguous() extends AmbiguityFeedback
  case class Unambiguous() extends AmbiguityFeedback
  case class AmbiguityWitness(ant: Nonterminal, w: Word) extends AmbiguityFeedback {
    override def toString = {
      "Ambiguous nonterminal: " + ant + " word: " + wordToString(w)
    }
  }
  case class AmbiguityWitnessWithTrees(ant: Nonterminal, w: Word, ptrees: List[ParseTree]) extends AmbiguityFeedback {
    //    override def toString = {
    //      "Ambiguous nonterminal: " + ant + " word: " + wordToString(w) + "parse trees: " + 
    //      
    //    }
  }
}

class AmbiguityChecker[T](g: Grammar[T])
	(implicit gctx: GlobalContext,
	    opctx: AmbiguityContext,
	    enumctx: EnumerationContext) {

  import AmbiguityChecker._

  def checkAmbiguityInStudentGrammar(): List[AmbiguityFeedback] = {

    val maxSize = opctx.maxSize
    val ng = g.fromCNF  
    val wordGen = new SizeBasedRandomAccessGenerator(ng, maxSize)

    val now = opctx.consecWordsForAmbiguityCheck
    val consecChecker = checkForDuplicates(wordGen, now) _
    //collect all the non-terminals in post-order 
    val nontermsInPO = GrammarUtils.postOrder(ng)

    //for every size from 1 to 'maxWordSize' check if there is an ambiguous word       
    var ambiguities = List[AmbiguityWitness]()
    var break = false
    for (size <- 1 to maxSize) if (!break) {
      nontermsInPO.foreach { nt =>
        consecChecker(nt, size) match {
          case aw: AmbiguityWitness =>
            ambiguities :+= aw
            break = true
          case _ => ;
        }
      }
    }
    ambiguities
  }
  
  def checkForAmbiguity(startSize: Int = 1, fastmode: Boolean = false): List[AmbiguityFeedback] = {

    //This generator is shared by nonterminals
    val maxSize = opctx.maxSize
    val wordGen = new SizeBasedRandomAccessGenerator(g, maxSize)

    val nos = opctx.sampledWordsForAmbiguityCheck
    val now = opctx.consecWordsForAmbiguityCheck
    val sampleChecker = sampleBasedChecker(wordGen, nos)
    val consecChecker = checkForDuplicates(wordGen, now) _

    //collect all the non-terminals in post-order 
    val nontermsInPO = GrammarUtils.postOrder(g)

    if (opctx.debugAmbiguityCheck)
      println("Post-order: " + nontermsInPO.mkString(","))

    //for every size from 1 to 'maxWordSize' check if there is an ambiguous word   
    var exploredNonterms = Set[Nonterminal]() //set of nonterms that are found to be ambiguous
    var ambiguities = List[AmbiguityWitness]()

    for (size <- startSize to maxSize) {
      if (!gctx.abort) {
        //for stats
        gctx.stats.updateCumStats(1, "AmbWitSizeInc")
        
        nontermsInPO.filterNot(exploredNonterms.contains).foreach { nt =>
          //println("Checking non-terminal: " + nt + " for size: " + size)          
          if (!gctx.abort) {        
            consecChecker(nt, size) match {
              case aw: AmbiguityWitness =>
                                
                //log ambiguous words to console and file
                //println("Found ambiguous word: " + aw)
                gctx.logMessage("Found ambiguous word: " + aw)
                //for stats
                gctx.stats.updateCounter(1, "AmbNts")
                gctx.stats.updateCounterStats(aw.w.size, "WitSize", "AmbNts")

                exploredNonterms += nt
                ambiguities :+= aw

              case u: Unambiguous =>
                exploredNonterms += nt
              case _ if !fastmode =>
                val spaceSize = wordGen.wordCounter.boundForNonterminal(nt, size)
                if (spaceSize > nos) {
                  //here use the sample based checker
                  sampleChecker(nt, size) match {
                    case aw: AmbiguityWitness =>
                      //log ambiguous words to console and file
                      //println("Found ambiguous word (by sampling): " + aw)
                      gctx.logMessage("Found ambiguous word (by sampling): " + aw)
                      //for stats
                      gctx.stats.updateCounter(1, "AmbNts")
                      gctx.stats.updateCounterStats(aw.w.size, "WitSize", "AmbNts")

                      exploredNonterms += nt
                      ambiguities :+= aw
                    case _ => ; //possibly unambiguous here
                  }
                }
              case _ => ;
            }
          }
        }
      }
    }    
    ambiguities
  }

  def checkForDuplicates[T](wordGen: SizeBasedRandomAccessGenerator[T], now: Int)(nt: Nonterminal, size: Int) = {    
    //we can use bloom filters here if needed
    var words = Set[Word]()
    var duplicate: Option[Word] = None
    val seqEnum = wordGen.getSeqEnumerator(nt, size, now)
    var break = false

    while (!break && seqEnum.hasNext) {
      val w = seqEnum.next
      if (words.contains(w)) {
        duplicate = Some(w)
        break = true //found a duplicate so break and report the string as a witness
      } else
        words += w
    }
    //println("word at index " + (index - step) + " : " + wordToString(words.toList(words.size - 1)))
    if (duplicate.isDefined) {            
      AmbiguityWitness(nt, duplicate.get)
    } else
      PossiblyUnambiguous()
    /* Note saying unambiguous in this case is slightly hard
      else if (words.size < now)
        Unambiguous() //here, we have enumerated all words belonging to the grammar
*/ }

  /**
   * nos - Number of samples
   */
  def sampleBasedChecker[T](wordGen: SizeBasedRandomAccessGenerator[T], nos: Int) = {
    //create a cnf grammar starting at each non-terminal    
    val cnfg = g.cnfGrammar
    val cykParsers = g.nonTerminals.map { nt =>
      (nt -> new CYKParser(CNFConverter.removeUnreachableRules(Grammar(nt, cnfg.rules))))
    }.toMap

    (nt: Nonterminal, size: Int) => {
      //create a CYK parser
      val cykparser = cykParsers(nt)
      val spaceSize = wordGen.wordCounter.boundForNonterminal(nt, size)
      //      println("# words of size " + size+" : "+spaceSize)      
      //make sure that we atleast need 10 bits
      if (nos >= spaceSize) {
        //the range is smaller than the required number of samples, use the normal checker itself
        checkForDuplicates(wordGen, nos)(nt, size)
      } else {
        val sampleEnum = wordGen.getSamplingEnumerator(nt, size, nos)
        var feedback: AmbiguityFeedback = PossiblyUnambiguous()
        var break = false
        while (!break && sampleEnum.hasNext) {
          //get a random number with at most rangeBits number of bits    
          val w = sampleEnum.next
          //println("word #: " + index+" word size: "+w.size)          
          //check if the 'w' has two parse trees
          cykparser.hasMultipleTrees(w) match {
            case None =>
              ; //do nothing                
            case Some((nt, substr, choices)) =>
              //log ambiguous words to console and file
              /*val msg = "Found ambiuous non-terminal: " + nt + " ambiguous substr: " +
                wordToString(substr) + "Possible parsing choices: " + choices
              //println(msg)
              opctx.logMessage(msg)*/              

              feedback = AmbiguityWitness(nt, substr)
              break = true
          }
        }
        feedback
      }
    }
  }

  /*  import generators.GrammarBoundingHelper._
  */
  /**
   * Bounded Ambiguity Checker
   * Note: if space is problem we can use bloom filters
   */ /*
  def checkForBoundedAmbiguity(g: Grammar, bg: Grammar)(implicit opctx: OperationContext): AmbiguityFeedback = {

    if (opctx.debugAmbiguityCheck) {
      println("Grammar: " + bg)
    }
     
    //This generator is shared by nonterminals
    val wordGen = new RandomAccessGenerator(bg)
    val maxWordSize = opctx.maxWordSize

    def checkForDuplicates(nt: Nonterminal, now: Int, step: Int) = {
      var index = -1
      var words = MutableMap[Word, BigInt]()
      var duplicate: Option[Word] = None
      var break = false
      while (words.size <= now && !break) {

        //increment index in steps
        index += step
        if (opctx.debugAmbiguityCheck) {
          if (index % 100 == 0)
            println("word #: " + index)
        }
        wordGen.getWordAtIndexNonterminal(nt, index) match {
          case Element(w) if (words.contains(w)) =>
            duplicate = Some(w)
            break = true //found a duplicate so break and report the string as a witness
          case Element(w) =>
            if (opctx.debugAmbiguityCheck) {
              if (index % 1000 == 0)
                println(nt + " word at index " + index + " : " + wordToString(w))
            }
            if (w.size > maxWordSize)
              break = true
            else
              words += (w -> index)
          case _ =>
            //break and say that the nonterminal is not ambiguous
            break = true
        }
      }
      //println("word at index " + (index - step) + " : " + wordToString(words.toList(words.size - 1)))
      if (duplicate.isDefined) {
        val w = duplicate.get
        //translate the parse trees to use the non-terminals of the unbounded grammar
        val ptree1 = remapParseTree(wordGen.constructParseTreeForNonterminal(nt, words(w)).get)
        val ptree2 = remapParseTree(wordGen.constructParseTreeForNonterminal(nt, index).get)
        AmbiguityWitnessWithTrees(nt, w, List(ptree1, ptree2))

      } else if (words.size < now)
        Unambiguous() //here, we have enumerated all words belonging to the grammar
      else
        PossiblyUnambiguous()
    }
    //collect all the non-terminals in post-order 
    val nontermsInPO = GrammarUtils.postOrder(bg)

    if (opctx.debugAmbiguityCheck)
      println("Post-order: " + nontermsInPO.mkString(","))

    //for each non-terminal generate some words (consecutively and also with sampling )
    // and check if a word repeats or if it has two parse trees.
    //TODO: print the derivation
    nontermsInPO.foldLeft(PossiblyUnambiguous(): AmbiguityFeedback) {
      case (res: AmbiguityWitness, _) =>
        res
      case (res @ AmbiguityWitnessWithTrees(ant, w, ptrees), cnt) =>
        //print the ambiguous string and continue
        println("Found ambiguous word: " + wordToString(w))
        //print two parse trees for the word                   
        println("Parse tree 1: " + ParseTreeUtils.parseTreetoString(ptrees(0)))
        println("Parse tree 2: " + ParseTreeUtils.parseTreetoString(ptrees(1)))

        println("Checking non-terminal: " + cnt)
        val now = opctx.consecWordsForAmbiguityCheck
        //generate 'now' consecutive words for the non-terminal
        checkForDuplicates(cnt, now, 1)

      case (acc, nt) =>
        //        /if (opctx.debugAmbiguityCheck)
          println("Checking non-terminal: " + nt)
          
        val now = opctx.consecWordsForAmbiguityCheck
        //generate 'now' consecutive words for the non-terminal
        checkForDuplicates(nt, now, 1)
    }
  }*/
}