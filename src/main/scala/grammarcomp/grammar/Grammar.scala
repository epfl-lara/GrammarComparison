package grammarcomp

package grammar

import utils._
import scala.collection.mutable.{ Map => MutableMap }
import scala.language.implicitConversions

object CFGrammar {

  object SymbolId {
    var symId: Long = 0
    def newId = synchronized {
      val oldId = symId
      symId += 1
      oldId
    }
  }
  
  trait Symbol[+T] {// symbol is covariant in T
     // DSL operations
    def ~[W >: T, U <: W](sym: Symbol[U]): Symbols[W] = Symbols(this :: List[Symbol[W]](sym))
    def |[W >: T, U <: W](syms: Symbols[U]) = Alternatives(Symbols(List(this)) :: List[Symbols[W]](syms))
    def |[W >: T, U <: W](sym: Symbol[U]) = Alternatives(List[Symbols[W]](Symbols(List(this)), Symbols(List(sym))))
  }
  
  /**
   * The following operations are only used by the DSL
   */
  case class Symbols[+T](syms: List[Symbol[T]]) {   
    def ~[W >: T, U <: W](sym: Symbol[U]) = Symbols((syms: List[Symbol[W]]) :+ sym)
    def |[W >: T, U <: W](alt: Symbol[U]) = Alternatives(this :: List(Symbols(List[Symbol[W]](alt))))
    def |[W >: T, U <: W](syms: Symbols[U]) = Alternatives(this :: List[Symbols[W]](syms))
  }
  
  case class Alternatives[+T](rhsList: List[Symbols[T]]) {
    def |[W >: T, U <: W](alt: Symbol[U]) = Alternatives(rhsList :+ Symbols(List[Symbol[W]](alt)))
    def |[W >: T, U <: W](syms: Symbols[U]) = Alternatives((rhsList : List[Symbols[W]]) :+ syms)
  }

  /**
   * We use object factories to reduce the memory pressure while 
   * testing large programs.
   */
  object Nonterminal {
    //a big factory for non-terminals
    val nonterminalFactory = MutableMap[scala.Symbol, Nonterminal]()        
    def apply(sym: scala.Symbol): Nonterminal = {
      nonterminalFactory.getOrElse(sym, {
        val nt = new Nonterminal(sym)
        nonterminalFactory += (sym -> nt)
        nt
      })
    }

    def unapply(nt: Nonterminal): Option[scala.Symbol] = {
      Some(nt.sym)
    }
  }

  class Nonterminal(val sym: scala.Symbol) extends Symbol[Nothing] {
    override def hashCode = sym.hashCode()
    override def equals(other: Any) = {
      other match {
        case other: Nonterminal => this.sym == other.sym
        case _ => false
      }
    }
    val name = sym.name
    override def toString =  sym.toString
    // DSL operations
    def ::=[T](rhses: Alternatives[T]) = rhses.rhsList.map(rhs => Rule(this, rhs.syms))
    def ::=[T](rhs: Symbols[T]) = Rule(this, rhs.syms)
    def ::=[T](rhs: Symbol[T]) = Rule(this, List(rhs))    
  }
  
  /**
   * An trait that marks Terminals that represent classes of terminals.
   * E.g. Identifier is a terminal that represents a set of identifiers
   */
  trait TerminalClass {
    def terminalClass: Any = this.getClass()
  }
 
  /**
   * generic terminals 
   */
  class Terminal[T](val obj: T) extends Symbol[T] {
    override def hashCode = obj.hashCode()
    override def equals(other: Any) = {
      other match {
        case other: Terminal[T] => this.obj == other.obj
        case _ => false
      }
    }
    override def toString = obj.toString        
  }

  //a big factory for creating terminals
  object Terminal {    
    val terminalFactory = MutableMap[String, StringTerminal]()
    def apply[T](obj: T): Terminal[T] = {
      obj match {
        case name: String =>
          terminalFactory.getOrElse(name, {
            val t = new StringTerminal(SymbolId.newId, name)
            terminalFactory += (name -> t)
            t
          }).asInstanceOf[Terminal[T]]
        case _ =>
          new Terminal(obj)
      }
    }
    
    def unapply[T](t: Terminal[T]): Option[T] = {
      Some(t.obj)
    }
  }
  
  /**
   *  A hashconsed terminals used for representing string valued terminals.
   *  Note: all string valued terminals  will use this class (an invariant of the system)
   */
  class StringTerminal(val id: Long, val name: String) extends Terminal[String](name) {
    override def hashCode = id.toInt
    override def equals(other: Any) = {
      other match {
        case other: StringTerminal => this.id == other.id
        case _ => false
      }
    }
    override def toString = name        
  }  
  
  /*object Rule {
    def apply[T](leftSide: Nonterminal, rightSide: List[Symbol[T]]) = 
      new Rule(leftSide, rightSide) 
    
    def unapply[T](r: Rule[T]): Option[(scala.Symbol, List[Any])] = {
      Some((r.leftSide.sym, r.rightSide.map { 
        case nt: Nonterminal => nt.sym
        case t: Terminal[T] => t.obj
      }))      
    }   
  }*/

  case class Rule[+T](val leftSide: Nonterminal, val rightSide: List[Symbol[T]]) {

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
            case t: Terminal[_] =>             
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
    
  case class Rules[+T](l: List[Rule[T]])
  implicit def ruleToRules[T](rl: Rule[T]) = Rules(List(rl))
  implicit def listToRules[T](l: List[Rule[T]]) = Rules(l) 
  
  object Grammar {
    /**
     * Only used by the DSL
     */
    def apply[T](st: Nonterminal, rules: List[Rules[T]]): Grammar[T] = {
      Grammar(st, Rules(rules.flatMap(_.l)))
    }
  }  
  
  /**
   * The type parameter is the type of the terminal
   */
  case class Grammar[T](start: Nonterminal, grammarRules: Rules[T]) {

    val rules = grammarRules.l
    //used for efficiency
    lazy val nontermToRules = this.rules.groupBy(_.leftSide)
    lazy val nonTerminals = this.rules.map(_.leftSide).distinct
    lazy val terminals = this.rules.flatMap(_.rightSide.collect { case t: Terminal[T] => t }).toSet
    lazy val nontermsInPostOrder = GrammarUtils.postOrder(this)
    lazy val twonfGrammar = CNFConverter.to2NF(this)
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
  def prettyPrint[T](g: Grammar[T]): Grammar[T] = {
    simplifyGrammar(renameAutoSymbols(g))
  }

  /**
   * Preserves the ordering of the rules
   */
  def rulesToStr[T](rules: List[Rule[T]]) = {
    val groupedRules = rules.groupBy(_.leftSide)
    val lefts = rules.map(_.leftSide).distinct
    lefts.foldLeft("") {
      case (acc, nt) =>
        acc + "\n" + nt + " -> " + groupedRules(nt).map(_.rightSideToString).mkString("", " | ", "")
    }
  }

  type SententialForm[T] = List[Symbol[T]]
  type Word[T] = List[Terminal[T]]
  type Words[T] = List[Word[T]]
  type SententialForms[T] = List[SententialForm[T]]

  def copy(s: Nonterminal): Nonterminal = {
    Nonterminal(scala.Symbol(Util.freshName(Some(s.name))))
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
        (i: Int) => !newNonterms.contains(Nonterminal(scala.Symbol(idPart + i))))(0)
      val newnt = Nonterminal(scala.Symbol(idPart + index))
      newNonterms += newnt
      acc + ((nt, newnt))
    })
    replaceMap
  }

  /*def renameAutoSymbols(nonterms: Set[Nonterminal], rules: List[Rule]) : List[Rule] ={    
    replace(rules, genRenameMap(nonterms)) 
  }*/
  
  def isInternalNonterminal(nt: Nonterminal) = nt.name.contains('-')

  /**
   * Important: this function has to be deterministic.
   * That is, given the syntactically same grammar, every non-terminal should
   * be mapped to the same fresh nonterminal
   */
  def renameAutoSymbols[T](g: Grammar[T]): Grammar[T] = {
    val hyphenatedSyms = g.rules.map(_.leftSide).distinct.filter(_.name.contains('-'))
    //use smaller indices for nonterminals    
    replace(g, genRenameMap(hyphenatedSyms, nonterminals(g)))
  }

  /**
   * Replaces a nonterminal by another nonterminal.
   */
  def replace[U](rules: List[Rule[U]], replaceMap: Map[Nonterminal, Nonterminal]): List[Rule[U]] = {

    def repSym[T <: Symbol[U]](s: T): T = s match {
      case nt: Nonterminal => replaceMap.getOrElse(nt, nt).asInstanceOf[T]
      case t: Terminal[U] => t.asInstanceOf[T]
    }
    val newrules = rules.map {
      case Rule(l, rhs) => Rule(repSym(l), rhs.map(repSym))
    }
    newrules
  }

  def replace[T](g: Grammar[T], replaceMap: Map[Nonterminal, Nonterminal]): Grammar[T] = {
    val newrules = replace(g.rules, replaceMap)
    val newstart = replaceMap.getOrElse(g.start, g.start)
    Grammar[T](newstart, newrules)
  }

  def rightSides[T](nt: Nonterminal, g: Grammar[T]) = {
    g.nontermToRules(nt).map(_.rightSide).distinct
  }

  def nontermsInRightSide[T](rule: Rule[T]): List[Nonterminal] = {
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
  def simplifyGrammar[T](ing: Grammar[T]): Grammar[T] = {
    val unitNTs = ing.nontermToRules.collect {
      case (nt, List(Rule(_, rside))) if nt != ing.start && !rside.contains(nt) => nt
    }.toSet
    val inlinedG = Util.fixpoint((g: Grammar[T]) => inlineNonterms(unitNTs, g))(ing)

    //drop unitNTs productions from the rules
    val newrules = inlinedG.rules.collect {
      case rule @ Rule(lhs, _) if !unitNTs.contains(lhs) => rule
    }
    Grammar[T](inlinedG.start, newrules)
  }

  /**
   * TODO: change this to inline nonterminals only at selected indices
   */
  def inlineNontermsInSententialForm[T](nontermsToInline: Set[Nonterminal],
    sententialForm: List[Symbol[T]], g: Grammar[T]): List[List[Symbol[T]]] = {
    //      /println("candidate rules: "+candRules)      
    def inlineNonterms(sform: List[Symbol[T]]): List[List[Symbol[T]]] = {
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

  def inlineNontermsInRules[T](nontermsToInline: Set[Nonterminal], rules: List[Rule[T]], g: Grammar[T]) = {
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
  def inlineNonterms[T](nontermsToInline: Set[Nonterminal], g: Grammar[T]): Grammar[T] = {
    Grammar[T](g.start, inlineNontermsInRules(nontermsToInline, g.rules, g))
  }

  def terminals[T](g: Grammar[T]): Set[Terminal[T]] = {
    g.terminals
  }

  def nonterminals[T](g: Grammar[T]): Set[Nonterminal] = {
    //g.rules.map(_.leftSide).toSet
    g.nonTerminals.toSet
  }

  /**
   * Checks if 'dest' is reachable from 'src' in one or more steps
   */
  def reach[T](g: Grammar[T], src: Nonterminal, dest: Nonterminal): Boolean = {

    var visited = scala.collection.mutable.Set[Symbol[T]](src)
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
  def epsilonFree[T](g: Grammar[T]): Boolean = {
    val epsilonProducers = g.rules.collect { case Rule(l, List()) => l }.toSet
    if (epsilonProducers.isEmpty)
      true
    else if (epsilonProducers == Set(g.start))
      g.rules.forall(rule => !rule.rightSide.contains(g.start))
    else
      false
  }

  sealed abstract class NormFeedback[+T] 
  case class Correct() extends NormFeedback
  case class Error[T](rule: Rule[T], msg: String) extends NormFeedback

  def isNormalized[T](g: Grammar[T], checkStart: Boolean = true) = 
    g.rules.forall(r => isRuleNormalized(g.start, r, checkStart) == Correct())  

  def isRuleNormalized[T](start: Nonterminal, rule: Rule[T], checkStart: Boolean = true): NormFeedback[T] = {
    rule match {
      case Rule(leftSide, rightSide) => {
        val rightSideOk =
          if (rightSide.size == 0) //this is an epsilon production    	    
            if (leftSide == start)
              Correct()
            else
              Error(rule, "Only start symbol can produce empty string")
          else if (rightSide.size == 1) //no unit productions 
            if (rightSide(0).isInstanceOf[Terminal[T]])
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

  def isRuleIn2NF[T](g: Grammar[T], rule: Rule[T], checkStart: Boolean): NormFeedback[T] = rule match {
    case Rule(leftSide, rightSide) =>
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
        Error[T](rule, "Right side has more than two symbols")
  }

  def foldFeedback[T](g: Grammar[T], feedbackFun: Rule[T] => NormFeedback[T]) = {
    g.rules.foldLeft(None: Option[Error[T]]) {
      case (None, rule) =>
        feedbackFun(rule) match {
          case Correct()     => None
          case err: Error[T] => Some(err)
        }
      case (fb, rule) => fb
    }
  }

  /**
   * Checks if a grammar is in CNF form
   */
  def isInCNF[T](g: Grammar[T], checkStart: Boolean = true): Boolean = !getRuleNotInCNF(g, checkStart).isDefined
  
  def isIn2NF[T](g: Grammar[T], checkStart: Boolean = true): Boolean = !getRuleNotIn2NF(g, checkStart).isDefined
  
  /**
   * When checkStart is set to true it will always consider the start symbol
   * appearing on the right-side as an error, irrespective of whether it is
   * nullable or not.
   */
  def getRuleNotInCNF[T](g: Grammar[T], checkStart: Boolean = true): Option[Error[T]] = {

    def isRuleInCNF(rule: Rule[T]): NormFeedback[T] = rule match {
      case Rule(leftSide, rightSide) =>
        isRuleNormalized(g.start, rule, checkStart) match {
          case Correct() => isRuleIn2NF(g, rule, checkStart)           
          case other => other
        }
    }
    foldFeedback(g, isRuleInCNF)    
  }

  def getRuleNotIn2NF[T](g: Grammar[T], checkStart: Boolean = true): Option[Error[T]] = {    
    foldFeedback(g, isRuleIn2NF(g, _, checkStart))    
  }
  
  def getRuleNotInGNF[T](g: Grammar[T]): Option[Error[T]] = {
    def isRuleInGNF(rule: Rule[T])  = rule match {
      case Rule(leftSide, rightSide) =>
        isRuleNormalized(g.start, rule) match {
          case Correct() =>
            if (rightSide.size < 2)
              Correct()
            else {
              if (rightSide(0).isInstanceOf[Terminal[T]])
                Correct()
              else Error(rule, "Right side should start with a terminal")
            }
          case other =>
            other
        }
    }
    foldFeedback(g, isRuleInGNF)
  }

  def isInGNF[T](g: Grammar[T]) = !getRuleNotInGNF(g).isDefined

  def wordToString[T](sententialForm: List[Symbol[T]]) = {
    if (sententialForm.isEmpty)
      "\"\""
    else
      sententialForm.mkString(" ")
  }

  def wordsToString[T](words: Seq[List[Symbol[T]]]) = {
    words.map(wordToString).mkString("\n")
  }

  /**
   * A semantics preserving algorithm for eliminating some non-terminals from the
   * input grammar (and the subset of rules of the grammar)
   * to the *maximum* extent possible.
   */
  def removeNonterminals[T](cnfG: Grammar[T], irules: List[Rule[T]], nonterms: List[Nonterminal]) = {
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
          (Grammar[T](g.start, newGRules), newRules)
        }
      case (acc, _) =>
        acc
    }
  }

  def nontermUses[T](g: Grammar[T], nonterm: Nonterminal): Int = {
    g.rules.map(_.rightSide.count(_ == nonterm)).sum
  }

  def unitNTs[T](g: Grammar[T]) = {
    g.nontermToRules.collect {
      case (nt, List(Rule(_, rside))) if nt != g.start && !rside.contains(nt) => nt
    }
  }
  
  def freshNonterminal(nameOpt: Option[String] = None): Nonterminal = {
    Nonterminal(scala.Symbol(Util.freshName(nameOpt)))
  }

  /**
   * Creates a new grammar by appending a suffix to every non-terminal in the grammar 
   */
  def appendSuffix[T](suffix: String, g: Grammar[T]): Grammar[T] = {
    val newrules = g.rules.map {
      case Rule(lnt: Nonterminal, rhs) =>
        val newrhs = rhs.map {
          case nt: Nonterminal => Nonterminal(scala.Symbol(nt.name + suffix))
          case t: Terminal[T] => t
        }
        Rule(Nonterminal(scala.Symbol(lnt.name + suffix)), newrhs)
    }    
    val news = Nonterminal(scala.Symbol(g.start.name + suffix))
    Grammar[T](news, newrules)
  }
}
