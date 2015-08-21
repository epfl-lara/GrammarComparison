package grammar

import utils._
import scala.collection.mutable.{ Map => MutableMap }

object CFGrammar {

  object SymbolId {
    var symId: Long = 0
    def newId = synchronized {
      val oldId = symId
      symId += 1
      oldId
    }
  }

  object Nonterminal {
    //a big factory for non-terminals
    val nonterminalFactory = MutableMap[String, Nonterminal]()
    def apply(name: String): Nonterminal = {
      nonterminalFactory.getOrElse(name, {
        val nt = new Nonterminal(SymbolId.newId, name)
        nonterminalFactory += (name -> nt)
        nt
      })
    }

    def unapply(nt: Nonterminal): Option[String] = {
      Some(nt.name)
    }
  }

  trait Symbol {
    def toUniqueString: String
  }

  class Nonterminal(val id: Long, val name: String) extends Symbol {
    override def hashCode = id.toInt
    override def equals(other: Any) = {
      other match {
        case other: Nonterminal => this.id == other.id
        case _ => false
      }
    }
    override def toString = {
      name
    }
    def toUniqueString = name + id
  }

  object Terminal {
    //a big factory for terminals
    val terminalFactory = MutableMap[String, Terminal]()
    def apply(name: String): Terminal = {
      terminalFactory.getOrElse(name, {
        val t = new Terminal(SymbolId.newId, name)
        terminalFactory += (name -> t)
        t
      })
    }

    def unapply(t: Terminal): Option[String] = {
      Some(t.name)
    }
  }

  class Terminal(val id: Long, val name: String) extends Symbol {
    override def hashCode = id.toInt
    override def equals(other: Any) = {
      other match {
        case other: Terminal => this.id == other.id
        case _ => false
      }
    }
    override def toString = {
      name
    }
    def toUniqueString = name + id
  }

  /*abstract class Symbol(val name: String) {
    override def toString = name   
  }
  case class Terminal(override val name: String) extends Symbol(name) {
    //override def toString = name + "."
  }
  case class Nonterminal(override val name: String) extends Symbol(name) {
  }*/

  case class Rule(leftSide: Nonterminal, rightSide: List[Symbol]) {

    lazy val hash = leftSide.hashCode * 41 + rightSide.hashCode
    override def hashCode: Int = hash

    override def equals(other: Any) = other match {
      case Rule(l, r) => (l == this.leftSide && r == this.rightSide)
      case _ => false
    }

    def rightSideToString = {
      //replace all reserved character with quotes
      rightSide match {
        case List() => "\"\""
        case rside =>
          val pattern = """[\|\*\+\(\)\?]""".r
          rside.map {
            case t: Terminal =>
              val symstr = t.toString
              pattern.findFirstIn(symstr) match {
                case None => symstr
                case _ => "'" + symstr + "'"
              }
            case nt =>
              nt.toString
          }.mkString(" ")
      }
    }
    override def toString = {
      leftSide + " -> " +
        (if (rightSide.isEmpty) "\"\""
        else
          rightSideToString)
    }
  }

  case class Grammar(start: Nonterminal, rules: List[Rule]) {

    //used for efficiency
    lazy val nontermToRules = this.rules.groupBy(_.leftSide)
    lazy val nonTerminals = this.rules.map(_.leftSide).distinct
    lazy val terminals = this.rules.flatMap(_.rightSide.collect { case t: Terminal => t }).toSet
    lazy val nontermsInPostOrder = GrammarUtils.postOrder(this)
    lazy val cnfGrammar = CNFConverter.toCNF(this)
    lazy val fromCNF = CNFConverter.cnfToGrammar(cnfGrammar)

    def longString = {
      //always make the start symbol rules appear first
      val str = nontermToRules(start).mkString("\n")
      nontermToRules.keys.filterNot(_ == start).foldLeft(str)((acc, nt) =>
        acc + nontermToRules(nt).mkString("\n"))
    }

    def productionsToStr(nt: Nonterminal): String = {
      //replace all reserved character with quotes
      val rulesStr = nontermToRules(nt).map(_.rightSideToString)
      nt + " -> " + rulesStr.mkString("", " | ", "\n")
    }

    override def toString = {
      if (rules.isEmpty)
        ""
      else {
        val str = productionsToStr(start)
        nontermToRules.keys.filterNot(_ == start).foldLeft(str)((acc, nt) =>
          acc + productionsToStr(nt))
      }
    }

    def toHTMLString = {
      val str = "<br/>" + productionsToStr(start) + "<br/>"
      nontermToRules.keys.filterNot(_ == start).foldLeft(str)((acc, nt) =>
        acc + productionsToStr(nt) + "<br/>")
    }
  }

  /**
   * Pretty prints a grammar
   */
  def prettyPrint(g: Grammar): Grammar = {
    simplifyGrammar(renameAutoSymbols(g))
  }

  /**
   * Preserves the ordering of the rules
   */
  def rulesToStr(rules: List[Rule]) = {
    val groupedRules = rules.groupBy(_.leftSide)
    val lefts = rules.map(_.leftSide).distinct
    lefts.foldLeft("") {
      case (acc, nt) =>
        acc + "\n" + nt + " -> " + groupedRules(nt).map(_.rightSideToString).mkString("", " | ", "")
    }
  }

  type SententialForm = List[Symbol]
  type Word = List[Terminal]
  type Words = List[Word]
  type SententialForms = List[SententialForm]

  def copy(s: Nonterminal): Nonterminal = {
    Nonterminal(Util.freshName(Some(s.name)))
  }

  /**
   * Maps a given set of nonterminals to fresh names.
   * The new name is picked so that it is distinct from the names of nonterminals
   * in the set 'allNonterminal'
   * Important: this function has to be deterministic as it may be used in a deterministic way
   * That is, given the syntactically same grammar, every non-terminal should
   * be mapped to the same fresh nonterminal
   */
  def genRenameMap(nonterms: List[Nonterminal], allNonterms: Set[Nonterminal]): Map[Nonterminal, Nonterminal] = {
    var newNonterms = allNonterms -- nonterms
    val replaceMap = nonterms.foldLeft(Map[Nonterminal, Nonterminal]())((acc, nt) => {
      //extract the text part of the nonterminal's name
      //println("NT name: "+nt.name+" last Index of 0: "+nt.name.lastIndexOf('0'))
      val alphanumPart = nt.name.substring(0, nt.name.indexOf('-'))
      val idPart = if (alphanumPart.last.isDigit)
        alphanumPart.dropRight(1)
      else alphanumPart
      //increment index as long as no 'nonterminal' of that name "idPart + index" exists
      val index = Util.repeatUntil((i: Int) => i + 1,
        (i: Int) => !newNonterms.contains(Nonterminal(idPart + i)))(0)
      val newnt = Nonterminal(idPart + index)
      newNonterms += newnt
      acc + (nt -> newnt)
    })
    replaceMap
  }

  /*def renameAutoSymbols(nonterms: Set[Nonterminal], rules: List[Rule]) : List[Rule] ={    
    replace(rules, genRenameMap(nonterms)) 
  }*/

  /**
   * Important: this function has to be deterministic.
   * That is, given the syntactically same grammar, every non-terminal should
   * be mapped to the same fresh nonterminal
   */
  def renameAutoSymbols(g: Grammar): Grammar = {
    val hyphenatedSyms = g.rules.map(_.leftSide).distinct.filter(_.name.contains('-'))
    //use smaller indices for nonterminals    
    replace(g, genRenameMap(hyphenatedSyms, nonterminals(g)))
  }

  /**
   * Replaces a nonterminal by another nonterminal.
   */
  def replace(rules: List[Rule], replaceMap: Map[Nonterminal, Nonterminal]): List[Rule] = {

    def repSym[T <: Symbol](s: T): T = s match {
      case nt: Nonterminal => replaceMap.getOrElse(nt, nt).asInstanceOf[T]
      case t: Terminal => t.asInstanceOf[T]
    }
    val newrules = rules.map {
      case Rule(l, rhs) => Rule(repSym(l), rhs.map(repSym))
    }
    newrules
  }

  def replace(g: Grammar, replaceMap: Map[Nonterminal, Nonterminal]): Grammar = {
    val newrules = replace(g.rules, replaceMap)
    val newstart = replaceMap.getOrElse(g.start, g.start)
    Grammar(newstart, newrules)
  }

  def rightSides(nt: Nonterminal, g: Grammar) = {
    g.nontermToRules(nt).map(_.rightSide).distinct
  }

  def nontermsInRightSide(rule: Rule): List[Nonterminal] = {
    rule.rightSide.collect { case nt: Nonterminal => nt }
  }

  /**
   * Performs the following simplifications to the grammar
   * a) If there is only one non-recursive production for a nonterminal then replace it with its production
   * Note that if A -> a S and S -> a A and if both A and S do not have any other productions then
   * A and S are unproductive nonterminals. The implementation relies on this fact.
   * TODO: other simplifications
   * if there are two non-terminals with equivalent productions they can be replaced
   * rest ?
   */
  def simplifyGrammar(ing: Grammar): Grammar = {
    val unitNTs = ing.nontermToRules.collect {
      case (nt, List(Rule(_, rside))) if nt != ing.start && !rside.contains(nt) => nt
    }.toSet
    val inlinedG = Util.fixpoint((g: Grammar) => inlineNonterms(unitNTs, g))(ing)

    //drop unitNTs productions from the rules
    val newrules = inlinedG.rules.collect {
      case rule @ Rule(lhs, _) if !unitNTs.contains(lhs) => rule
    }
    Grammar(inlinedG.start, newrules)
  }

  /**
   * TODO: change this to inline nonterminals only at selected indices
   */
  def inlineNontermsInSententialForm(nontermsToInline: Set[Nonterminal],
    sententialForm: List[Symbol],
    g: Grammar): List[List[Symbol]] = {
    //      /println("candidate rules: "+candRules)      
    def inlineNonterms(sform: List[Symbol]): List[List[Symbol]] = {
      val index = sform.indexWhere(sym => sym match {
        case nt: Nonterminal => nontermsToInline.contains(nt)
        case _ => false
      }, 0)

      if (index >= 0) {
        val symbolToInline = sform(index).asInstanceOf[Nonterminal]
        val prefix = sform.take(index) //all elements before the 'index'        
        val afterIndex = sform.drop(index + 1) //this will also drop the element at the 'index'        
        val suffixes = inlineNonterms(afterIndex)
        for (
          body <- rightSides(symbolToInline, g);
          suf <- suffixes
        ) yield {
          prefix ++ body ++ suf
        }
      } else
        List(sform)
    }
    inlineNonterms(sententialForm)
  }

  def inlineNontermsInRules(nontermsToInline: Set[Nonterminal], rules: List[Rule], g: Grammar) = {
    val newrules = rules.flatMap {
      case rule @ Rule(leftSide, rightSide) =>
        inlineNontermsInSententialForm(nontermsToInline, rightSide, g).map(rs => Rule(leftSide, rs))
    }.distinct
    newrules
  }

  /**
   * Inline all rules of the form N -> \alpha where N is a nonterminal in nontermsToInline.
   * Note that the following procedure does not perform iterative inlining.
   * In particular symbols in \alpha contained in nontermsToInline are not inlined.
   * If iterative inlining is required, the clients can invoke this procedure iteratively.
   * The output and input grammars should be equivalent.
   */
  def inlineNonterms(nontermsToInline: Set[Nonterminal], g: Grammar): Grammar = {
    Grammar(g.start, inlineNontermsInRules(nontermsToInline, g.rules, g))
  }

  def terminals(g: Grammar): Set[Terminal] = {
    g.terminals
  }

  def nonterminals(g: Grammar): Set[Nonterminal] = {
    //g.rules.map(_.leftSide).toSet
    g.nonTerminals.toSet
  }

  /**
   * Checks if 'dest' is reachable from 'src' in one or more steps
   */
  def reach(g: Grammar, src: Nonterminal, dest: Nonterminal): Boolean = {

    var visited = scala.collection.mutable.Set[Symbol](src)
    def reachRec(nt: Nonterminal): Boolean = {
      g.nontermToRules(nt).exists {
        case Rule(_, rightSide) =>
          rightSide.exists {
            case next: Nonterminal if next == dest =>
              true
            case next: Nonterminal if visited.contains(next) =>
              false
            case next: Nonterminal =>
              visited.add(next)
              reachRec(next)
            case _ => false
          }
      }
    }
    reachRec(src)
  }

  /**
   * Two properties need to be satisfied
   * (a) No symbol other than the start produces epsilon
   * (b) The start symbol does not occur on the right hand side if it produces epsilon
   */
  def epsilonFree(g: Grammar): Boolean = {
    val epsilonProducers = g.rules.collect { case Rule(l, List()) => l }.toSet
    if (epsilonProducers.isEmpty)
      true
    else if (epsilonProducers == Set(g.start))
      g.rules.forall(rule => !rule.rightSide.contains(g.start))
    else
      false
  }

  sealed abstract class NormFeedback
  case class Correct() extends NormFeedback
  case class Error(rule: Rule, msg: String) extends NormFeedback

  def isNormalized(g: Grammar) = g.rules.forall(r => isRuleNormalized(g.start, r) == Correct())

  def isRuleNormalized(start: Nonterminal, rule: Rule, checkStart: Boolean = true): NormFeedback = {
    rule match {
      case Rule(leftSide, rightSide) => {
        val rightSideOk =
          if (rightSide.size == 0) //this is an epsilon production    	    
            if (leftSide == start)
              Correct()
            else
              Error(rule, "Only start symbol can produce empty string")
          else if (rightSide.size == 1) //no unit productions 
            if (rightSide(0).isInstanceOf[Terminal])
              Correct()
            else
              Error(rule, "Unit production")
          else if (!rightSide.contains(start)) //right side should not contain start
            Correct()
          else {
            if (checkStart)
              Error(rule, "start symbol occurs on the right side")
            else {
              Correct()
            }
          }
        rightSideOk
      }
    }
  }

  /**
   * Checks if a grammar is in CNF form
   */
  def isInCNF(g: Grammar, checkStart: Boolean = true): Boolean = !getRuleNotInCNF(g, checkStart).isDefined

  /**
   * When checkStart is set to true it will always consider the start symbol
   * appearing on the right-side as an error, irrespective of whether it is
   * nullable or not.
   */
  def getRuleNotInCNF(g: Grammar, checkStart: Boolean = true): Option[Error] = {

    def isRuleInCNF(rule: Rule, checkStart: Boolean) = rule match {
      case Rule(leftSide, rightSide) =>
        isRuleNormalized(g.start, rule, checkStart) match {
          case Correct() =>
            if (rightSide.size < 2)
              Correct()
            else if (rightSide.size == 2) {
              if (!rightSide.forall(_.isInstanceOf[Nonterminal]))
                Error(rule, "Right side of production has terminals")
              else if (!checkStart) {
                //check if the grammar does not have start symbol on the right right side if it is nullable
                if (rule.rightSide.contains(g.start) &&
                  g.rules.contains(Rule(g.start, List())))
                  Error(rule, "Nullable start symbol appears on the right-side of the rule")
                else
                  Correct()
              } else
                Correct()
            } else
              Error(rule, "Right side has more than two symbols")
          case other =>
            other
        }
    }
    g.rules.foldLeft(None: Option[Error]) {
      case (None, rule) =>
        isRuleInCNF(rule, checkStart) match {
          case Correct() =>
            None
          case err: Error =>
            Some(err)
        }
      case (fb, rule) => fb
    }
  }

  def getRuleNotInGNF(g: Grammar): Option[Error] = {
    def isRuleInGNF(rule: Rule) = rule match {
      case Rule(leftSide, rightSide) =>
        isRuleNormalized(g.start, rule) match {
          case Correct() =>
            if (rightSide.size < 2)
              Correct()
            else {
              if (rightSide(0).isInstanceOf[Terminal])
                Correct()
              else Error(rule, "Right side should start with a terminal")
            }
          case other =>
            other
        }
    }
    g.rules.foldLeft(None: Option[Error]) {
      case (None, rule) =>
        isRuleInGNF(rule) match {
          case Correct() =>
            None
          case err: Error =>
            Some(err)
        }
      case (fb, rule) => fb
    }
  }

  def isInGNF(g: Grammar) = !getRuleNotInGNF(g).isDefined

  def wordToString(sententialForm: List[Symbol]) = {
    if (sententialForm.isEmpty)
      "\"\""
    else
      sententialForm.mkString(" ")
  }

  def wordsToString(words: Seq[List[Symbol]]) = {
    words.map(wordToString).mkString("\n")
  }

  /**
   * A semantics preserving algorithm for eliminating some non-terminals from the
   * input grammar (and the subset of rules of the grammar)
   * to the *maximum* extent possible.
   */
  def removeNonterminals(cnfG: Grammar, irules: List[Rule], nonterms: List[Nonterminal]) = {
    //Note: do not remove start
    nonterms.filterNot(_ == cnfG.start).foldLeft((cnfG, irules)) {
      case ((g, rules), nonterm) if g.nonTerminals.contains(nonterm) =>
        val ntprods = g.nontermToRules(nonterm).toSet
        if (ntprods.exists(_.rightSide.contains(nonterm))) {
          //this is a self-recursive nonterminal, it is not possible to remove this while preserving semantics
          (g, rules)
        } else {
          //inline this nonterminal in each of its use
          val inGRules = inlineNontermsInRules(Set(nonterm), g.rules, g)
          val inrules = inlineNontermsInRules(Set(nonterm), rules, g)

          //additionally, if nonterm occurs in the left side of the rules, 
          //uses all the rules that use nonterms
          val transRules = if (inrules.exists(_.leftSide == nonterm)) {
            val relRules = g.rules.filter(_.rightSide.contains(nonterm))
            inrules.flatMap {
              case Rule(l, r) if (l == nonterm) =>
                relRules.map {
                  case Rule(rell, relr) =>
                    val newr = relr.flatMap {
                      case nt: Nonterminal if nt == nonterm => r
                      case sym => List(sym)
                    }
                    Rule(rell, newr)
                }
              case rule @ _ => List(rule)
            }
          } else
            inrules

          //remove 'ntprods' from both newGRules and newrules
          val newGRules = inGRules.distinct.filterNot(ntprods.contains)
          val newRules = transRules.distinct.filterNot(ntprods.contains)
          (Grammar(g.start, newGRules), newRules)
        }
      case (acc, _) =>
        acc
    }
  }

  def nontermUses(g: Grammar, nonterm: Nonterminal): Int = {
    g.rules.map(_.rightSide.count(_ == nonterm)).sum
  }

  def unitNTs(g: Grammar) = {
    g.nontermToRules.collect {
      case (nt, List(Rule(_, rside))) if nt != g.start && !rside.contains(nt) => nt
    }
  }

  /**
   * Creates a new grammar by appending a suffix to every non-terminal in the grammar 
   */
  def appendSuffix(suffix: String, g: Grammar): Grammar = {
    val newrules = g.rules.map {
      case Rule(Nonterminal(lname), rhs) =>
        val newrhs = rhs.map {
          case Nonterminal(rname) => Nonterminal(rname + suffix)
          case t: Terminal => t
        }
        Rule(Nonterminal(lname + suffix), newrhs)
    }
    val Nonterminal(sname) = g.start
    val news = Nonterminal(sname + suffix)
    Grammar(news, newrules)
  }
}
