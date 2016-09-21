package equivalence
import grammar._
import repair._
import parsing._
import generators._
import CFGrammar._
import EBNFGrammar._
import BNFConverter._
import utils.Logging._
import utils._
import scala.collection.mutable.LinkedHashSet

//TODO: is it necessary to change the condition of Btrans to l+2 instead of L 
//and make each of the right-hand-sides atmost 3 non-terminals 
object EquivalenceVerfier {
  var smallestGrammars = List[(Grammar[_], Grammar[_])]()
  var smallestSize: Option[Int] = None

  type Operator[T] = (SententialForms[T], SententialForms[T], Terminal[T]) => Relation[T]

  /**
   * TODO: Use a better model and do not inherit from Symbol
   * as if an instance of the class escapes, it may result in chaos
   * Hard-coding for LL(2) grammars.
   * Need to modify this code if we need to generalize to LL(k) grammar
   */
  sealed case class PRNonterminal[T](nt: Nonterminal, pr: Terminal[T]) extends Symbol[T] {
    override def toString = "[" + pr + "," + nt + "]"
    def toUniqueString = toString
  }

  /**
   * A symbol that represents unions of sentential forms that starts with a PRNonterminal
   */
  sealed case class GroupedVariable[T](sforms: SententialForms[T]) extends Symbol[T] {
    val splitForms = sforms.map { sf => (sf.head.asInstanceOf[PRNonterminal[T]], sf.tail) }
    override def toString = "[" + sforms.map(sf => sf.mkString(" ")).mkString("+") + "]"
    def toUniqueString = toString
  }

  def senformToString(sf: SententialForm[_]) = "[" + sf.mkString(" ") + "]"

  def senformsToString(sforms: SententialForms[_]) = {
    if (sforms.isEmpty) "()"
    else {
      val head :: tail = sforms
      val str = tail.foldLeft(senformToString(head))((acc, senform) =>
        acc + " \u222A " + senformToString(senform))
      if (sforms.size > 1)
        "(" + str + ")"
      else
        str
    }
  }

  def first[T](sf: SententialForm[T], g: Grammar[T]): List[Terminal[T]] = sf match {
    case (t: Terminal[T]) :: tail => List(t)
    case (nt: Nonterminal) :: tail => GNFUtilities.firstNT(nt, g)
    case (prnt: PRNonterminal[T]) :: tail => List(prnt.pr)
    case (gv: GroupedVariable[T]) :: tail =>
      gv.splitForms.map {
        case (PRNonterminal(_, pr), rest) => pr
      }
    case _ => List()
  }

  def concatSuffix[T](sforms: SententialForms[T], suff: SententialForm[T]) = {
    sforms.map(_ ++ suff)
  }

  def concatPrefix[T](prex: SententialForm[T], sforms: SententialForms[T]) = {
    sforms.map(prex ++ _)
  }

  def getRightSides[T](sym: Symbol[T], g: Grammar[T]): SententialForms[T] = sym match {
    case nt: Nonterminal => rightSides(nt, g)
    case PRNonterminal(nt, p) => rightSides(nt, g).filter(_.head == p)
    case gv @ GroupedVariable(_) => gv.splitForms.flatMap {
      case (prnt, rest) =>
        concatSuffix(getRightSides(prnt, g), rest)
    }
    case _ => List()
  }

  /**
   * Computes the derivative of the sentential form w.r.t to the word
   * This is defined only if the grammar is in GNF.
   * Track the prefix (expanded) and suffix (unexpanded) parts
   */
  def derivative[T](inputWord: List[Terminal[T]], isf: SententialForm[T], g: Grammar[T]): List[(SententialForm[T], SententialForm[T])] = {

    def derivativeRec(word: List[Terminal[T]], prefix: SententialForm[T], suffix: SententialForm[T]): List[(SententialForm[T], SententialForm[T])] = {
      if (word.isEmpty)
        List((prefix, suffix))
      else {
        prefix match {
          case List() =>
            if (suffix.isEmpty) List()
            else
              derivativeRec(word, List(suffix.head), suffix.tail)
          case (head: Terminal[T]) :: tail =>
            if (head == word.head) derivativeRec(word.tail, tail, suffix)
            else List()
          case head :: tail =>
            //the rightSides should start with a terminal as we are in GNF form 
            //(even for GroupedVariable and PRNonterminals)
            val dervs = getRightSides(head, g).collect {
              case rh :: rt if rh == word.head => rt
            }.map(_ ++ tail).flatMap(newSform => derivativeRec(word.tail, newSform, suffix))
            dervs
        }
      }
    }
    derivativeRec(inputWord, List(), isf)
  }

  /**
   * This computes a derivative of a sentential form but separates the
   * symbols that we not used in the derivative computation from the those
   * that were used
   */
  def derivativeWithCommonSuffix[T](word: Word[T], sform: SententialForm[T], g: Grammar[T]) = {
    derivative(word, sform, g) match {
      case List() => (List(), List(), List())
      case dervs =>
        //choose the common suffix of all the derivatives
        val minSuffixLen = dervs.map(_._2.size).min
        val alpha = sform.dropRight(minSuffixLen)
        val suff = sform.takeRight(minSuffixLen)
        val deltas = dervs.map(d => (d._1 ++ d._2).dropRight(minSuffixLen))
        (alpha, deltas, suff)
    }
  }

  def derivatives[T](word: Word[T], sforms: SententialForms[T], g: Grammar[T]) = {
    sforms.map(sform => derivativeWithCommonSuffix(word, sform, g))
  }

  /**
   * Converts a sentential form with extended symbols to
   * that without extended symbols.
   * Note: the resulting set of sentential forms could be huge
   */
  def removeExtendedSymbols[T](sform: SententialForm[T], g: Grammar[T]): List[SententialForm[T]] = {
    sform.foldLeft(List[SententialForm[T]](List[Symbol[T]]())) {
      case (acc, s @ (Terminal(_) | Nonterminal(_))) =>
        concatSuffix(acc, List(s))
      case (acc, sym) =>
        getRightSides(sym, g).flatMap(concatSuffix(acc, _))
    }.distinct
  }

  /**
   * Converts a grammar to GNF, uses specialized epsilon elimination 
   * procedure for LL(1) grammars, so that they are LL(2) after removing epsilons.
   * TODO: the special conversion worsens results when one of the grammars is not LL(1) :-( why ?   
   */
  def toGNF[T](g1: Grammar[T], g2 : Grammar[T])(implicit gctx: GlobalContext) = {
	if(GrammarUtils.isLL1(g1) && GrammarUtils.isLL1(g2)){
	  (GNFConverter.toGNF(LLEpslionEliminator.eliminateEpsilons(g1)),
	      GNFConverter.toGNF(LLEpslionEliminator.eliminateEpsilons(g2)))
	} else 
	  (GNFConverter.toGNF(g1), GNFConverter.toGNF(g2)) 
  }
  
  sealed abstract class Relation[T]
  //'lhs' is a sentential form of grammar1 and 'rhs' is the sentential form of grammar2
  //This checks for equivalence under the prefix 'w' 
  case class Equals[T](lhs: SententialForms[T], rhs: SententialForms[T], w: Terminal[T]) extends Relation[T] {
    override def toString = {
      val lstring = senformsToString(lhs)
      val rstring = senformsToString(rhs)
      lstring + "=_{" + w + "}" + rstring
    }
  }
  //'lhs' is a sentential form of grammar1 and 'rhs' is the sentential form of grammar2
  //This checks for inclusion of lhs in rhs under the prefix 'w' 
  case class Subset[T](lhs: SententialForms[T], rhs: SententialForms[T], w: Terminal[T]) extends Relation[T] {
    override def toString = {
      val lstring = senformsToString(lhs)
      val rstring = senformsToString(rhs)
      lstring + " \u2282_{" + w + "}" + rstring
    }
  }

  object BinaryOperator {
    def unapply[T](rel: Relation[T]): Option[(Operator[T], SententialForms[T], SententialForms[T], Terminal[T])] = rel match {
      case Equals(lhs, rhs, w) => Some((Equals[T], lhs, rhs, w))
      case Subset(lhs, rhs, w) => Some((Subset[T], lhs, rhs, w))
      case _ => None
    }
  }

  sealed abstract class TransformationResult[+T]
  case class NotApplicable[T](input: Relation[T]) extends TransformationResult[T] {
    override def toString = "<NA>"
  }
  case class Failure() extends TransformationResult { //the relation does not hold (this should not be possible)
    override def toString = "<fail>"
  }
  case class Success() extends TransformationResult {
    //the relation holds
    override def toString = "<pass>"
  }
  /*case class Any(subgoals: List[Relation]) extends TransformationResult {
    override def toString = { "<any> " + subgoals.mkString(", ") }
  }*/
  case class All[T](subgoals: List[Relation[T]]) extends TransformationResult[T] {
    override def toString = { "<all> " + subgoals.mkString(", ") }
  }
  //allows assuming the current relation in the context i.e, it can be taken as inductive hypothesis
  case class AllWithNewContext[T](subgoals: List[Relation[T]]) extends TransformationResult[T] {
    override def toString = { "<all> " + subgoals.mkString(", ") }
  }
}

class EquivalenceVerifier[T](ig1: Grammar[T], ig2: Grammar[T])
	(implicit gctx: GlobalContext, 
	    opctx: EquivalenceVerificationContext,
	    enumctx: EnumerationContext) {

  import EquivalenceVerfier._

  var stop = false
  var disableTests = false
  val timerTask = Util.scheduleTask(() => { stop = true },
    opctx.verificationTimeout * 1000)

  val alphabet = terminals(ig1).toList
  val (g1, g2) = {
    val (gnf1, gnf2) = toGNF(ig1, ig2)        
    (appendSuffix("1", gnf1), appendSuffix("2", gnf2))
  }  
  val ll2grammars = {
    //checks if both grammars are LL2, in which case we can use an optimized and complete Btrans
    GNFUtilities.isGNFGrammarLL2(g1) && GNFUtilities.isGNFGrammarLL2(g2)
  }

  val (g, genum) = {
    //union of g1 and g2
    val newstart = CFGrammar.freshNonterminal(Some("S"))
    val newrules = (Rule(newstart, List[Symbol[T]](g1.start)) +: g1.rules) ++ (Rule(newstart, List[Symbol[T]](g2.start)) +: g2.rules)
    val gram = Grammar[T](newstart, newrules)
    //inline the right-sides of the start non-terminals as they are unit productions
    val enumrules = g1.nontermToRules(g1.start).map(rl => Rule(newstart, rl.rightSide)) ++
      g2.nontermToRules(g2.start).map(rl => Rule(newstart, rl.rightSide)) ++
      g1.rules ++ g2.rules
    val enumGram = CNFConverter.simplify(Grammar[T](newstart, enumrules))
    (gram, enumGram)
  }
  val noTests = opctx.testsForVerification
  val maxSize = opctx.maxSizeForVerification
  val wordGen = new SizeBasedRandomAccessGenerator[T](genum, maxSize)
  val maxMinWord = {
    //'l' of the hopcroft algorithm
    nonterminals(genum).map(wordGen.getMinWord(_).get).maxBy(_.size)
  }

  def getMinWord(sform: SententialForm[T], prefix: Terminal[T]): Word[T] = {
    //get the derivative of 'sform' w.r.t 'prefix' 
    val dervs = derivative(List(prefix), sform, g).map(d => d._1 ++ d._2).flatMap {
      removeExtendedSymbols(_, g)
    }
    if (dervs.isEmpty) {
      throw new IllegalStateException("No derivative for the sentential form" +
        sform + " for  prefix: " + prefix)
    }
    val dervMinword = dervs.map {
      wordGen.getMinWord(_) match {
        case None =>
          throw new IllegalStateException("Cannot generate any word" +
            "for the sentential form: " + sform + " of length <= " + maxSize)
        case Some(w) =>
          w
      }
    }.minBy(_.size)
    val minword = prefix +: dervMinword
    minword
  }

  def genWords(wordGen: SizeBasedRandomAccessGenerator[T], nt: Nonterminal, nos: Int): List[Word[T]] = {
    //note we are using a sequential enumerator here
    val enums = (1 to maxSize).map { wordGen.getSeqEnumerator(nt, _, nos) }.toList
    var words = List[Word[T]]()
    //get one word from each size, until we get nos samples or we exhaust all enumerators
    var sizeQueue = (1 to maxSize).toList
    while (words.size < nos && !sizeQueue.isEmpty) {
      val size = sizeQueue.head
      val enum = enums(size - 1)
      if (enum.hasNext) {
        words :+= enum.next
        sizeQueue = sizeQueue.tail :+ size
      } else
        //every word of this size has been explored 
        sizeQueue = sizeQueue.tail
    }
    words
  }

  /**
   * Sample 'nos' words from the sentential form, starting with the given
   * prefix
   */
  def genWords(sform: List[Symbol[T]], nos: Int, prefix: Terminal[T]): List[Word[T]] = {
    //get the derivative of 'sform' w.r.t 'prefix' 
    val dervs = derivative(List(prefix), sform, g).map(d => d._1 ++ d._2).flatMap {
      removeExtendedSymbols(_, g)
    }
    if (dervs.isEmpty) {
      List() //the sentential form generates no words starting with the prefix
    } else {
      //create a grammar that contains all the sentential forms
      val nstart = CFGrammar.freshNonterminal(Some("Ts"))
      //note: we need to concatenate prefix to the derivatives
      val nrules = dervs.map(d => Rule(nstart, prefix +: d)) ++ genum.rules
      //unfortunately, adding support for enumerating sentential forms is hard
      //because the enumerator is not sufficiently parametrized.
      //TODO: The main overhead is re-evaluating the word-counter, can this be avoided ?
      val ng = Grammar[T](nstart, nrules)
      val wordGen = new SizeBasedRandomAccessGenerator[T](ng, maxSize)
      genWords(wordGen, nstart, nos)
    }
  }

  val cykParser = new CYKParser(g.cnfGrammar)
  def parseWithSententialForms(sforms: List[SententialForm[T]], words: Words[T]) = {
    val plainSforms = sforms.flatMap(removeExtendedSymbols(_, g))
    words.foldLeft(Some((List[Word[T]](), List[Word[T]]())): Option[(List[Word[T]], List[Word[T]])]) {
      case _ if gctx.abort =>
        None
      case (None, _) =>
        None
      case (Some((parsables, unparsables)), word) =>
        if (cykParser.parseWithSententialForms(plainSforms, word))
          Some(parsables :+ word, unparsables)
        else
          Some(parsables, unparsables :+ word)
    }
  }

  //in general, if a transformation fails to prove a relation, it does not mean a relation is wrong 
  /**
   * 'A' or 'branching' transformation. Assuming GNF form for grammars.
   * For this transformation to apply, we require the lhs one sentential-form
   * and rhs to be a single sentential for for equality.
   * Can we relax the precondition ?
   * If yes how does this interact with the conversion to standard relation form ?
   */
  def Atrans(rel: Relation[T]): TransformationResult[T] = rel match {
    case Equals(List(_), List(_), _) | Subset(List(_), _, _) =>
      val BinaryOperator(op, lhsSet @ List(_), rhsSet, w) = rel
      //here, we can safely assume that lhs and rhs start with 
      //nonterminals as the prefix terminals are stripped away           
      val newrels = alphabet.map(b => {
        def expand(sforms: SententialForms[T]) = sforms.flatMap {
          case head :: tail =>
            //pick all productions of 'A' from which it is possible to derive 'wb'
            //note: for LL(k) grammars there will be unique such productions                                   
            val rightSidesForWB = getRightSides(head, g).foldLeft(List[SententialForm[T]]()) {
              case (acc, rside) =>
                val derv = derivative(List(w, b), rside ++ tail, g)
                if (derv.isEmpty) acc
                else acc :+ rside.tail
            }
            concatSuffix(rightSidesForWB, tail)
          case _ =>
            List() //here, sform is empty, hence we cannot generate any derivative for the sform          
        }.toList
        val newlhs = expand(lhsSet)
        val newrhs = expand(rhsSet)
        //perform the replacement step here
        val replacedLHS = newlhs.flatMap {
          case lhs @ _ if lhs(0).isInstanceOf[Terminal[T]] || lhs(0).isInstanceOf[Nonterminal] =>
            List(lhs)
          case PRNonterminal(nt, p) :: tail if p == b =>
            List(nt :: tail)
          case (gv @ GroupedVariable(_)) :: tail =>
            val sformsWithB = gv.splitForms.collect {
              case (PRNonterminal(nt, p), tail) if p == b =>
                nt :: tail
            }
            concatSuffix(sformsWithB, tail)
          case _ => List()
        }
        //construct the new prefix string dropping  'w' and adding 'b'
        val newPrefix = b
        op(replacedLHS, newrhs, newPrefix)
      })
      AllWithNewContext(newrels)
    case _ =>
      NotApplicable(rel)
  }

  def Btrans: Relation[T] => TransformationResult[T] = {
    if (ll2grammars)
      BtransLL2
    else
      BtransNonLL2
  }

  def restrictSForms(sforms: List[SententialForm[T]], b: Terminal[T]) = {
    sforms.collect {
      case (head: Nonterminal) :: tail if first(List(head), g).contains(b) =>
        PRNonterminal(head, b) +: tail
      case (head: Terminal[T]) :: tail if head == b => tail
    }
  }
  /**
   * B transformation or split transformation for LL(2).
   * Here, both the lhs and rhs are assumed to be singleton
   */
  def BtransLL2(rel: Relation[T]): TransformationResult[T] = rel match {
    case Equals(List(lhs), List(rhs), w) =>
      if (lhs.size > maxMinWord.size) { //this is the condition used by Hopcroft, Korenjak       
        val gamma = lhs.tail
        //here, we can assume that lhs starts with a non-terminal
        val head = lhs.head match {
          case nt: Nonterminal =>
            (nt.asInstanceOf[Symbol[T]], getMinWord(List(nt), w)) //the shortest word should have size  k-1, but that will hold if it has 'w' as a prefix          
        }
        val A = head._1
        val Aminword = head._2
        val firstOfGamma = first(gamma, g).toList
        //derive minWord of A from beta
        val (alpha, deltas, suf) = derivativeWithCommonSuffix(Aminword, rhs, g)
        if (opctx.debugBtrans) {
          println("Aminword: " + wordToString(Aminword))
          println(s"alpha: $alpha deltas: ${deltas.mkString(",")} suf: $suf")
        }
        //the triple means 'alpha' became 'delta's (after derivation) but 'suf' remained the same    
        //suf can be further split into 'rho' and 'beta' if need be
        val SiRelations = firstOfGamma.map(b => {
          //select only those 'sforms' that contain a 'b' in their first
          val bdervs = concatSuffix(deltas, suf).filter { d => first(d, g).contains(b) }
          if (bdervs.size > 1)
            throw new IllegalStateException(s"$rhs has more then one right hand side for ${Aminword :+ b}")
          Equals(List(gamma), bdervs, b)
        })

        if (SiRelations.exists(_.rhs.isEmpty)) {
          gctx.stats.updateCumStats(1, "BtransFailures")
          Failure()
        } else {
          val (rho, beta) = suf match {
            case List() => //suf is empty
              (List(), List())
            case head :: tail =>
              (List(head), tail)
          }
          val rsforms = firstOfGamma.map { b =>
            val deltasRho = concatSuffix(deltas, rho)
            restrictSForms(deltasRho, b).head //head must exist because of the if condition.
          }
          val M = GroupedVariable(rsforms)
          val S2Relation = Equals(List(List(A, M)), List(alpha ++ rho), w)
          AllWithNewContext(SiRelations :+ S2Relation)
        }
      } else
        NotApplicable(rel)
    case _ =>
      throw new IllegalStateException(s"Found relation $rel during LL(1) verification")
  }

  //we need to have a rule that checks if firsts agree
  /**
   * B transformation for non-LL1 grammars.
   * The algorithm is sound but incomplete.
   * In the sequel, we can safely assume that there are no grouped non-terminals
   */
  var noImmediateBtrans: Option[Relation[T]] = None
  def BtransNonLL2(rel: Relation[T]): TransformationResult[T] = rel match {
    case Equals(List(_), List(_), _) | Subset(List(_), _, _) if noImmediateBtrans != Some(rel) =>
      //reset noImmediateBtrans
      noImmediateBtrans = None
      val BinaryOperator(op, List(lhs), rhsList, w) = rel
      if (lhs.size > maxMinWord.size) { //this is condition used by Hopcroft, Korenjak
        val gamma = lhs.tail
        val head = lhs.head match {
          case nt: Nonterminal =>
            (nt.asInstanceOf[Symbol[T]], getMinWord(List(nt), w))
          //the shortest word should have size  k-1, but that will hold if it has 'w' as a prefix
          case prnt @ PRNonterminal(nt, x) if x == w =>
            (prnt.asInstanceOf[Symbol[T]], getMinWord(List(nt), w))
        }
        val A = head._1
        val Aminword = head._2

        if (opctx.debugBtrans)
          println("Aminword: " + wordToString(Aminword))
        val firstOfGamma = first(gamma, g).toList
        val rhsDervs = derivatives(Aminword, rhsList, g) //list of (alpha,Delta,beta)        
        val bToDervs = firstOfGamma.map(b => {
          val dervs = rhsDervs.flatMap {
            case (alpha, deltas, suf) =>
              concatSuffix(deltas, suf)
          }
          //select only those 'sforms' that contain a 'b' in their first
          val bdervs = dervs.filter { d => first(d, g).contains(b) }
          (b, bdervs)
        })
        val SiRelations = bToDervs map {
          case (b, bdervs) =>
            op(List(gamma), bdervs, b)
        }

        //construct a type 2 relation (or S relation) given deltasRho and alphaRho
        def constructSRelation(deltasRho: List[SententialForm[T]],
          alphaRho: SententialForm[T]): Relation[T] = {
          val restrictedDeltasRhos = firstOfGamma.flatMap(b => {
            restrictSForms(deltasRho, b)
          })
          val lhs = concatPrefix(List(A), restrictedDeltasRhos)
          op(lhs, List(alphaRho), w)
        }
        var addToContext = true
        val SRelations = rhsDervs.map {
          case (alpha, deltas, suf) =>
            //compute delta rho
            suf match {
              case List() =>
                //here, there is no suffix left, which means "monotonicity" does not hold    
                noImmediateBtrans = Some(rel)
                addToContext = false
                constructSRelation(deltas, alpha)
              case rho :: tail if deltas.exists(_.isEmpty) =>
                val (deltasRho, alphaRho) = (concatSuffix(deltas, List(rho)), alpha :+ rho)
                tail match {
                  case List() =>
                    //here again monotonicity does not hold    
                    noImmediateBtrans = Some(rel)
                    addToContext = false
                    constructSRelation(deltasRho, alphaRho)
                  case _ =>
                    //here monotonicity holds
                    constructSRelation(deltasRho, alphaRho)
                }
              case tail =>
                //here, monotonicity holds                
                constructSRelation(deltas, alpha)
            }
        }
        val newrels = SiRelations ++ SRelations
        //add to context only if for every derv. we have a suffix of size >= 2
        //this is being a somewhat  conservative. 
        //TODO: optimize this, and also do distribute all the conditions in SRelations,
        //group together things that have the same "suffix"
        //        val addToContext = rhsDervs.forall {
        //          case (alpha, deltas, suf) => suf.size > 1
        //        }
        if (addToContext)
          AllWithNewContext(newrels)
        else {
          All(newrels)
        }
      } else
        NotApplicable(rel)
    case _ =>
      NotApplicable(rel)
  }

  /**
   * A transformation that handles epsilons in the relations
   */
  val emptyForm = List[Symbol[T]]()
  def epsilonTrans(rel: Relation[T]): TransformationResult[T] = rel match {
    case Equals(lset, rset, w) =>
      if (lset.isEmpty && rset.isEmpty)
        Success()
      else if (lset.isEmpty || rset.isEmpty)
        Failure() //only one set is empty
      else if (lset.contains(emptyForm) && rset.contains(emptyForm)) {
        All(List(Equals(lset.filterNot(_ == emptyForm), rset.filterNot(_ == emptyForm), w)))
      } else if (lset.contains(emptyForm) || rset.contains(emptyForm)) {
        Failure()
      } else {
        NotApplicable(rel)
      }
    case Subset(lset, rset, w) =>
      if (lset.isEmpty)
        Success()
      else if (rset.isEmpty)
        Failure() //right is empty while left isn't
      else if (lset.contains(emptyForm) && rset.contains(emptyForm)) {
        All(List(Equals(lset.filterNot(_ == emptyForm), rset.filterNot(_ == emptyForm), w)))
      } else if (lset.contains(emptyForm) && !rset.contains(emptyForm)) {
        Failure() //here epsilon is derivable in the left but not on the right
      } else if (rset.contains(emptyForm)) {
        All(List(Subset(lset, rset.filterNot(_ == emptyForm), w))) //here epsilon can be removed
      } else {
        NotApplicable(rel)
      }
  }

  /**
   * A reduction based on the lengths
   */
  //a temporary counter 
  var usedTestcases = false

  def lengthTrans(rel: Relation[T]): TransformationResult[T] = {

    def minSForm(sforms: SententialForms[T]) = {
      sforms.min(Ordering.by((sform: SententialForm[T]) => sform.size))
    }

    def minWordSize(sforms: SententialForms[T], prefix: Terminal[T]) = {
      sforms.map(getMinWord(_, prefix).size).min
    }

    rel match {
      case Equals(lset, rset, w) =>
        //the following are some termination checks        
        if (minWordSize(lset, w) < minSForm(rset).size)
          Failure()
        else if (minWordSize(rset, w) < minSForm(lset).size)
          Failure()
        else
          NotApplicable(rel)

      case Subset(lset, rset, w) =>
        val minlwordSize = minWordSize(lset, w)
        if (minlwordSize < minSForm(rset).size)
          Failure()
        else if (opctx.useTestcasesInVerification && !disableTests
          && rset.exists(_.size > minlwordSize)) {
          //here, try to heuristically remove all larger terms in the 'rset' by 
          //checking if the 'lhs' is completely subsumed in the 'rset'
          rel match {
            case Subset(List(lhs), rset, w) =>
              val lwords = genWords(lhs, noTests, w).toSet
              //choose only those 'rsets' that have size <= the size of the minword of lhs
              val smallRset = rset.filter(_.size <= minlwordSize)
              val rwords = smallRset.flatMap(rform => {
                //generate four times the no. of tests so that there is a better chance of 
                //covering all of the lwords        
                genWords(rform, 4 * noTests, w).toSet
              })
              val uninclWords = (lwords -- rwords).toSet
              /*println("rforms: "+wordsToString(smallRset))
              println("Unincluded words: "+wordsToString(uninclWords.toList))*/
              parseWithSententialForms(smallRset, uninclWords.toList) match {
                case None =>
                  NotApplicable(rel)
                case Some((parsableWords, unparsableWords)) =>
                  if (unparsableWords.isEmpty) {
                    //here, everything is subsumed. So we can remove the larger terms from 'rset'
                    //Though this could be an approximation
                    usedTestcases = true
                    val newrel = Subset(lset, smallRset, w)
                    All(List(newrel))
                  } else
                    NotApplicable(rel)
              }
            case _ =>
              NotApplicable(rel)
          }
        } else NotApplicable(rel)
    }
  }

  def distributeOverInclusion(rel: Relation[T]): TransformationResult[T] = rel match {
    case Subset(lforms, rhs, w) if (lforms.size > 1) =>
      val newrels = lforms.map(lform => Subset(List(lform), rhs, w))
      All(newrels)
    case _ =>
      NotApplicable(rel)
  }

  def equalityToInclusion(rel: Relation[T]): TransformationResult[T] = rel match {
    case Equals(lhs, rhs, w) =>
      //when the control reaches here both lhs and rhs have atleast two sentential forms
      val newrels = List(Subset(lhs, rhs, w), Subset(rhs, lhs, w))
      All(newrels)
    case _ => NotApplicable(rel)
  }

  /**
   * This is a lossy transformation as it is driven by counter-examples.
   * TODO: this makes certain verification proofs not go through ?
   */
  def simplifyUsingTestcases(rel: Relation[T]): TransformationResult[T] =
    rel match {
      case Subset(List(lhs), rset, w) if rset.size > 1 =>
        val lwords = genWords(lhs, noTests, w).toSet
        val rhsMap = rset.map(rform => {
          //generate four times the no. of tests so that there is a better chance of 
          //covering all of them        
          val rwords = genWords(rform, 4 * noTests, w).toSet
          val uninclWords = (lwords -- rwords)
          parseWithSententialForms(List(rform), uninclWords.toList) match {
            case Some((parsableWords, unparsableWords)) =>
              rform -> (rwords ++ parsableWords)
            case None =>
              rform -> (rwords ++ uninclWords)
          }
        }).toMap
        val rwords = rhsMap.values.flatten.toSet
        if (!lwords.subsetOf(rwords)) { //evaluate the relation
          if (opctx.debugTestcasesSimplification) {
            printDebugMessage("Testcases", "Total Lwords: " + lwords.size)
            printDebugMessage("Testcases", "Total unincluded words: " + (lwords -- rwords).size)
            printDebugMessage("Testcases", "Unincluded words: " + wordsToString((lwords -- rwords).toList))
            //printDebugMessage("Testcases","Total unnecessary words: "+wordsToString((rwords -- lwords).toList))
          }
          Failure()
        } else {
          //in this case we can check if some elements can be removed from rset
          var newrset = List[SententialForm[T]]()
          var uncoveredElements = lwords
          while (!uncoveredElements.isEmpty) {
            //choose a set from 'rset' that covers the most number of uncovered elements
            val rform = rset.minBy(rform => (uncoveredElements -- rhsMap(rform)).size)
            newrset :+= rform
            uncoveredElements --= rhsMap(rform)
          }
          if (newrset.size < rset.size) {
            usedTestcases = true
            All(List(Subset(List(lhs), newrset, w)))
          } else
            NotApplicable(rel) //no simplication is possible here                         
        }
      case _ => NotApplicable(rel)
    }

  def lift(f: Relation[T] => TransformationResult[T]): (TransformationResult[T] => TransformationResult[T]) = {
    val newf = (res: TransformationResult[T]) => res match {
      case NotApplicable(rel) => f(rel)
      case _ => res
    }
    newf
  }

  //for debugging
  var dumpTRes = false
  def dumpTResult(phaseName: String)(res: TransformationResult[T]): TransformationResult[T] = {
    if (opctx.debugEquivVerifier > 0) {
      res match {
        case NotApplicable(_) => res
        case _ if dumpTRes =>
          dumpTRes = false
          printDebugMessage(phaseName, res.toString)
          if (opctx.debugEquivVerifier > 1 && phaseName == "B")
            scala.io.StdIn.readLine()
          res
        case _ => res
      }
    } else res
  }

  //in the order of increasing priority  
  def transformations(context: Set[Relation[T]]) = {
    dumpTRes = true //an ungly way of managing state TODO: fix this

    epsilonTrans _ andThen
      dumpTResult("Epsilon") andThen
      lift(lengthTrans) andThen
      dumpTResult("Length") andThen
      {
        if (opctx.useTestcasesInVerification && !disableTests)
          lift(simplifyUsingTestcases)
        else
          (x: TransformationResult[T]) => x //id function
      } andThen
      dumpTResult("Testcases") andThen
      lift(Btrans) andThen
      dumpTResult("B") andThen
      lift(Atrans) andThen
      dumpTResult("A") andThen
      lift(distributeOverInclusion) andThen
      dumpTResult("Distribute") andThen
      lift(equalityToInclusion) andThen
      dumpTResult("ToInclusion")
  }

  //The following are filters on the results of transformations
  /**
   * This is expensive as it makes comparisons over lists
   * TODO: can this be made efficient ?
   */
  def makeDistinct(rels: List[Relation[T]]): List[Relation[T]] = {
    rels.map(rel => {
      val BinaryOperator(op, lset, rset, w) = rel
      op(lset.distinct, rset.distinct, w)
    }).distinct
  }

  var provenRelations = Set[Relation[T]]()
  def removeProvenRelations(rels: List[Relation[T]]): List[Relation[T]] = {
    rels.filterNot(provenRelations.contains _)
  }

  def removeImpliedRelations(context: Set[Relation[T]])(rels: List[Relation[T]]): List[Relation[T]] = {
    rels.filterNot(rel => rel match {
      case Subset(lhs, rhs, w) =>
        val rset = rhs.toSet
        context.exists {
          case ctxrel @ BinaryOperator(op, cl, cr, w) =>
            if (rel == ctxrel ||
              Equals(lhs, rhs, w) == ctxrel ||
              Equals(rhs, lhs, w) == ctxrel)
              true
            //else if (rhs.size > 1 && rset.subsetOf(cr.toSet))
            else if (rhs.size > 1 && cr.toSet.subsetOf(rset))
              true
            else
              false
        }
      case Equals(lset, rset, w) =>
        context.contains(rel) || context.contains(Equals(rset, lset, w))
    })
  }

  def filters(context: Set[Relation[T]]) = {
    makeDistinct _ andThen
      removeProvenRelations andThen
      removeImpliedRelations(context)
  }

  def verifyRelation(rel: Relation[T], context: Set[Relation[T]]): Option[Boolean] = {
    //if there has been a timeout and abort
    if (stop || gctx.abort) {
      if (opctx.printVerifcationFeedback)
        println((if (gctx.abort) "Verification Aborted!" else "Verification timed out!"))
      //for stats
      if (stop)
        gctx.stats.updateCumStats(1, "VeriTimeouts")
      None
    } else {
      val BinaryOperator(op, lhs, rhs, w) = rel

      //for stats
      gctx.stats.updateCounter(1, "EquivRelations")
      gctx.stats.updateCounterStats(rhs.size, "RelationRHSSize", "EquivRelations")

      if (opctx.debugEquivVerifier > 0)
        printDebugMessage("Verifying", rel.toString)

      val transRes = transformations(context)(rel)
      val res = transRes match {
        case NotApplicable(_) => //dont know what to do
          if (opctx.printVerifcationFeedback)
            println("No transformation applies to sub-relation: " + rel)
          None
        case Success() =>
          Some(true)
        case Failure() =>
          //print feedback
          if (opctx.printVerifcationFeedback)
            println("Cannot prove sub-relation: " + rel)
          Some(false)
        case All(_) | AllWithNewContext(_) =>
          val (rels, newcontext) = transRes match {
            case All(rels) =>
              (rels, context) //use the old context itself
            case AllWithNewContext(rels) =>
              (rels, context + rel)
            case _ =>
              throw new IllegalStateException("Impossible case!")
          }
          //val simpRels = filters(context)(rels)
          val simpRels = filters(newcontext)(rels) //filter using new context

          if (opctx.debugEquivVerifier > 0)
            printDebugMessage("Filtered", simpRels.mkString(", "))

          if (simpRels.isEmpty)
            Some(true) //success: all subgoals are established
          else {
            //val newcontext = context + rel
            //val head :: tail = simpRels
            val res = simpRels.foldLeft(Some(true): Option[Boolean])((acc, newrel) => acc match {
              case Some(true) =>
                val relRes = verifyRelation(newrel, newcontext)
                newrel match {
                  case Equals(lhs, rhs, w) if (relRes == Some(false)) && !ll2grammars =>
                    if (opctx.debugEquivVerifier > 0)
                      printDebugMessage("Failed to prove", newrel.toString())
                    //break this down in to new subrels, if we are not in the LL2 world                    
                    val subrels: List[Relation[T]] = List(Subset(lhs, rhs, w), Subset(rhs, lhs, w))
                    val subrelsToProve = filters(newcontext)(subrels)
                    subrelsToProve.foldLeft(Some(true): Option[Boolean])((acc, subrel) => acc match {
                      case Some(true) =>
                        verifyRelation(subrel, newcontext)
                      case _ => acc
                    })
                  case _ =>
                    //here, either the subrelation holds or we ran out of time
                    relRes
                }

              case _ => acc
            })
            res
          }
      }
      if (res == Some(true)) {
        //It is safe to assume 'rel' even if it relies on the hypothesized facts
        //as we are performing guarantee reasoning
        provenRelations += rel
      }
      res
    }
  }

  /**
   * Mainly used for debugging
   */
  /*def checkRelationsBetweenGrammars: Boolean = {
    implicit val equivctx = new EquivalenceCheckingContext()
    implicit val parsectx = new ParseContext()
    
    var equivTester = new StudentGrammarEquivalenceChecker(ig1)
    if (equivTester.isEquivalentTo(ig2).isEmpty) {
      printDebugMessage("Verifier", "Input grammars are equivalent")
      equivTester = new StudentGrammarEquivalenceChecker(g1)
      if (equivTester.isEquivalentTo(g2).isEmpty) {
        printDebugMessage("Verifier", "GNF grammars are equivalent")
        true
      } else {
        printDebugMessage("Verifier", "GNF grammars are not equivalent")
        false
      }
    } else {
      printDebugMessage("Verifier", "Input grammars are not equivalent")
      false
    }
  }*/

  def proveEquivalence(): Option[Boolean] = {
    //for stats
    gctx.stats.updateCounter(1, "VerifierCalls")
    val statstimer = new Stats.Timer()

    val startset1 = List(List(g1.start))
    val startset2 = List(List(g2.start))
    if (opctx.debugEquivVerifier > 0) {
      /*if (opctx.debugEquivVerifier > 1) {
        checkRelationsBetweenGrammars
      }*/
      printDebugMessage("Gr1", g1.toString)
      printDebugMessage("Gr2", g2.toString)
      printDebugMessage("Start", (g1.start, g2.start).toString)
    }
    //compute prefixes of length '1' (fixing that we always deal with LL(2) grammars)
    //from the start symbols
    val g1first = GNFUtilities.firstNT(g1.start, g)
    val g2first = GNFUtilities.firstNT(g2.start, g)
    val res = if (g1first.toSet != g2first.toSet) {
      //first set of start1 and start2 mismatch
      if (opctx.debugEquivVerifier > 0) {
        printDebugMessage("FirstMismatch", Util.setString(g1first) + Util.setString(g2first))
      }
      Some(false)
    } else {
      try {
        g1first.foldLeft(Some(true): Option[Boolean]) {
          case (Some(true), t) =>
            verifyRelation(Equals(startset1, startset2, t), Set()) match {
              case r @ Some(true) =>
                r
              case _ if (opctx.useTestcasesInVerification) =>
                //retry with out using test cases
                disableTests = true
                verifyRelation(Equals(startset1, startset2, t), Set())
              case r @ _ =>
                r
            }
          case (acc, _) =>
            acc
        }
      } catch {
        case e: StackOverflowError =>
          val msg = "StackOverflow"
          gctx.logMessage(msg)
          println(msg)
          None
        case e: Throwable =>
          throw e
      }
    }
    if (timerTask.isDefined)
      timerTask.get.cancel()

    //for stats
    gctx.stats.updateCounterTime(statstimer.timeWithoutGC, "VerificationTime", "VerifierCalls")

    if (GrammarUtils.isLL1(ig1) && GrammarUtils.isLL1(ig2)) {
      gctx.stats.updateCumStats(1, "BothLL1Grammars")
      if (res == Some(true))
        gctx.stats.updateCumStats(1, "SuccessWithLL1Grammars")
      //if the grammars in GNF form are LL1  
      if (GrammarUtils.isLL1(g1) && GrammarUtils.isLL1(g2)) {
        gctx.stats.updateCumStats(1, "LL1GNF")
        if (res == Some(true))
          gctx.stats.updateCumStats(1, "SuccessWithLL1GNF")
      }
    }
    if (ll2grammars) {
      gctx.stats.updateCumStats(1, "LL2GNF")
      if (res == Some(true))
        gctx.stats.updateCumStats(1, "SuccessWithLL2GNF")
    }

    //temporary code
    if (!ll2grammars && usedTestcases && res == Some(true)) {
      if (!smallestSize.isDefined || g.rules.size < smallestSize.get) {
        smallestSize = Some(g.rules.size)
        smallestGrammars = List((ig1, ig2))
      } else if (g.rules.size == smallestSize.get) {
        smallestGrammars :+= (ig1, ig2)
      }
      //otherwise do nothing
    }
    res
  }
}