package grammar

import utils.Util

object BNFConverter {
  import CFGrammar._
  import EBNFGrammar._
  import BNFConverter._

  def usesRegOp(bnf: BNFGrammar) = {
    
    def hasRegOp(regex: RegExp) : Boolean = regex match {
      case RegOr(subs) => 
        subs.exists(hasRegOp)
      case RegConcat(subs) =>  
        subs.exists(hasRegOp)
      case RegId(_) | RegEmpty() => false      
      case _ => true
    }
    bnf.rules.exists(rule => hasRegOp(rule.rightSide))
  }
  
  /**
   * Recovering the BNF form after the conversion to normal grammar.
   */
  /*var generatedRegExp = Map[Symbol, RegExp]()
  var enclosingContext = Map[Nonterminal, RegExp => BNFRule]()*/

  /**
   * Converts a grammar in BNF form to the grammar in plain form
   */
  def ebnfToGrammar(bnf: BNFGrammar): Grammar = {
    //code for recovery 
//    generatedRegExp = Map()
//    enclosingContext = Map()
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
        def regexToSymbol(re: RegExp): (Symbol, List[Rule]) = re match {
          case RegId(name) =>
            val symbol = (if (nonterms.contains(name))
              Nonterminal(name)
            else
              Terminal(name))

            //recovery code
            /*if (!generatedRegExp.contains(symbol: Symbol))
              generatedRegExp += (symbol: Symbol) -> re*/

            (symbol, List())
          case RegClosure(sube) => {
            //create a new nonterminal 'C'
            val S = getFreshNonterm("star")            
            
            //recovery code
            /*val replaceContext = (l: RegExp) => context(RegConcat(List(re, RegOption(l))))
            enclosingContext += S -> replaceContext
            generatedRegExp += S -> re*/

            //create new rules for closure
            val newrules = regexToRules(S, RegOr(List(RegEmpty(), RegConcat(List(sube, RegId(S.name))))))
            (S, newrules)
          }
          case r @ RegPlus(sube) => {
            //create a new nonterminal 'C'
            val S = getFreshNonterm("plus")
            
            //code for recovery
            //Given, S -> e S | e which is e+. Adding S -> a implies not e+ | a but e*a | e+ a?
            /*val replaceContext = (newReg: RegExp) =>
              context(RegOr(List(RegConcat(List(RegClosure(sube), newReg)), 
                  RegConcat(List(r, RegOr(List(newReg, RegEmpty())))))))
            enclosingContext += S -> replaceContext
            generatedRegExp += S -> re*/
            
            sube match {
              case sube: RegId =>
                val newrules = regexToRules(S, RegOr(List(sube, RegConcat(List(sube, RegId(S.name))))))
                (S, newrules)
              case _ =>
                //here, create a new symbol for sube
                val T = getFreshNonterm("t")                
                
                //recovery code
                /*val replaceContext2 = (l: RegExp) => context(RegPlus(l))
                enclosingContext += T -> replaceContext2
                generatedRegExp += T -> sube*/
                
                val newrules = regexToRules(T, sube) ++
                  regexToRules(S, RegOr(List(RegId(T.name), RegConcat(List(RegId(T.name), RegId(S.name))))))
                (S, newrules)
            }
          }
          case RegOption(sube) => {
            val S = getFreshNonterm("opt")
            
            //recovery code
            /*val replaceContext = (l: RegExp) => context(RegOr(List(RegEmpty(), sube, l)))
            enclosingContext += S -> replaceContext
            generatedRegExp += S -> re*/
            
            //create new rules for option
            val newrules = regexToRules(S, RegOr(List(RegEmpty(), sube)))
            (S, newrules)
          }
          case RegOr(subes) => {
            //here, or occurs inside the concatenation. Therefore we need to create new nonterminals
            val S = getFreshNonterm("or")
            //val replaceContext = (l: RegExp) => context(RegOr(subes ++ List(l)))                        
            
            //create new rules for closure
            val newrules = regexToRules(S, re)
            (S, newrules)
          }
          case RegConcat(_) => throw new InternalError("Should have been handled one level above")
          case RegEmpty() => throw new InternalError("Should have been handled one level above")
        }

        // From a Regexp, returns a list of symbols which if concatenated represent the regex 
        def regexToRightSide(re: RegExp): (List[Symbol], List[Rule]) = re match {
          case RegConcat(args) =>
            //var i = -1
            args.foldLeft((List[Symbol](), List[Rule]()))((acc, arg) => {
              /*i += 1
              val j = i
              val replaceContext = (l: RegExp) => l match {
                case RegConcat(l) => context(RegConcat(args.take(j) ++ l ++ args.drop(j + 1)))
                case e => context(RegConcat(args.take(j) ++ List(e) ++ args.drop(j + 1)))
              }*/
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
        def regexToRules(leftSide: Nonterminal, re: RegExp): List[Rule] = re match {
          case RegOr(args) =>
            //var i = -1
            args.flatMap(arg => {
              /*i += 1
              val j = i
              val replaceContext = (l: RegExp) => context(RegOr(args.take(j) ++ List(l) ++ args.drop(j + 1)))*/
              regexToRules(leftSide, arg)
            })
          case _ =>
            val (syms, newrules) = regexToRightSide(re)
            Rule(leftSide, syms) +: newrules
        }
        val nt = Nonterminal(leftSide.name)
        regexToRules(nt, rightSide)
      }
    }
    val cfg = Grammar(Nonterminal(bnf.start.name), allRules)
    //println("Generated Regexp: "+generatedRegExp.mkString("\n"))
    cfg
  }
}