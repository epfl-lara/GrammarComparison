package grammar

import utils.Util

object BNFConverter {
  import CFGrammar._
  import EBNFGrammar._
  import BNFConverter._

  def usesRegOp[T](bnf: BNFGrammar[T]) = {

    /**
     * Checks if the regex has star or option operaton which is not a part of context-free grammars
     */
    def hasRegOp(regex: RegExp[T]): Boolean = regex match {
      case RegOr(subs) =>
        subs.exists(hasRegOp)
      case RegConcat(subs) =>
        subs.exists(hasRegOp)
      case _: Sym[_] | RegEmpty() => false
      case _                      => true
    }
    bnf.rules.exists(rule => hasRegOp(rule.rightSide))
  }

  /**
   * Converts a grammar in BNF form to the grammar in plain form
   */
  def ebnfToGrammar[T](bnf: BNFGrammar[T]): Grammar[T] = {
    //create the set of nonterminals
    var nonterms = bnf.rules.map(_.leftSide.name).toSet
    def getFreshNonterm(name: String): Nonterminal = {
      val freshname = Util.freshName(Some(name))
      nonterms += freshname
      Nonterminal(freshname)
    }

    val allRules = bnf.rules.flatMap {
      case rule @ BNFRule(leftSide, rightSide) => {

        // From a Regexp, returns a symbol representing this regexp
        def regexToSymbol(re: RegExp[T]): (Symbol[T], List[Rule[T]]) = re match {
          case nid: NontermId =>
            if (!nonterms.contains(nid.name))
              throw new InvalidGrammarException(s"Nonterminal $nid does not have any rules!")
            (Nonterminal(nid.name), List())

          case s: Sym[T] =>
            s.obj match {
              case str: String if nonterms.contains(str) => // this is a non-terminal
                (Nonterminal(str), List())
              case o =>
                (Terminal(o.asInstanceOf[T]), List()) // note: here we simply cast to `T` (this should succeed)
            }

          case RegClosure(sube) =>
            //create a new nonterminal 'C'
            val S = getFreshNonterm("star")
            //create new rules for closure
            val newrules = regexToRules(S, RegOr(List(RegEmpty(), RegConcat(List(sube, new NontermId(S.name))))))
            (S, newrules)

          case r @ RegPlus(sube) =>
            //create a new nonterminal 'C'
            val S = getFreshNonterm("plus")
            sube match {
              case sube: Sym[_] =>
                val newrules = regexToRules(S, RegOr(List(sube, RegConcat(List(sube, new NontermId(S.name))))))
                (S, newrules)
              case _ =>
                //here, create a new symbol for sube
                val T = getFreshNonterm("t")
                val newrules = regexToRules(T, sube) ++
                  regexToRules(S, RegOr(List(new NontermId(T.name), RegConcat(List(new NontermId(T.name), new NontermId(S.name))))))
                (S, newrules)
            }

          case RegOption(sube) => {
            val S = getFreshNonterm("opt")
            //create new rules for option
            val newrules = regexToRules(S, RegOr(List(RegEmpty(), sube)))
            (S, newrules)
          }
          case RegOr(subes) => {
            //here, or occurs inside the concatenation. Therefore we need to create new nonterminals
            val S = getFreshNonterm("or")
            //create new rules for closure
            val newrules = regexToRules(S, re)
            (S, newrules)
          }
          case RegConcat(_) => throw new InternalError("Should have been handled one level above")
          case RegEmpty()   => throw new InternalError("Should have been handled one level above")
        }

        // From a Regexp, returns a list of symbols which if concatenated represent the regex 
        def regexToRightSide(re: RegExp[T]): (List[Symbol[T]], List[Rule[T]]) = re match {
          case RegConcat(args) =>
            args.foldLeft((List[Symbol[T]](), List[Rule[T]]()))((acc, arg) => {
              val (syms, newrules) = regexToRightSide(arg)
              (acc._1 ++ syms, acc._2 ++ newrules)
            })
          case RegEmpty() => (List(), List())
          case _ => {
            val (sym, newrules) = regexToSymbol(re)
            (List(sym), newrules)
          }
        }

        // From a rule leftSide -> Regexp, returns a list of corresponding rules.
        def regexToRules(leftSide: Nonterminal, re: RegExp[T]): List[Rule[T]] = re match {
          case RegOr(args) =>
            args.flatMap(regexToRules(leftSide, _))
          case _ =>
            val (syms, newrules) = regexToRightSide(re)
            Rule[T](leftSide, syms) +: newrules
        }
        val nt = Nonterminal(leftSide.toString)
        regexToRules(nt, rightSide)
      }
    }
    val cfg = Grammar[T](Nonterminal(bnf.start.toString), allRules)
    //println("Generated Regexp: "+generatedRegExp.mkString("\n"))
    cfg
  }
}