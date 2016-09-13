package grammar

import scala.util.parsing.combinator.RegexParsers
import CFGrammar._
import EBNFGrammar._
import language.postfixOps

class GrammarParser extends RegexParsers {
  override type Elem = Char
  
  //preventing newline from being treated as a whitespace
  override val whiteSpace = """[ \t]+""".r

  /**
   * Symbol definition
   */
  def symbol = """([^\s\|\*\+\(\)\?'])+""".r | "'" ~> """([^\s'])+""".r <~ "'"
  def nontermSymbol = """[a-zA-Z]([a-zA-Z0-9_])*""".r

  /**
   * Regular expression definition
   */
  def regId = symbol ^^ {
    case name if name == "\"\"" => RegEmpty()
    case name => new RegId(name)
  }
  def nontermRegId = nontermSymbol ^^ { case name => new RegId(name) }

  //Precedence: () precedes unary operators,*,?,| precedes '.' precedes '|'
  def atom = regId | ("(" ~> regExp <~ ")")
  def closure = ("*") ^^ { case _ => RegClosure.apply _ }
  def option = ("?") ^^ { case _ => RegOption.apply _ }
  def plus = ("+") ^^ { case _ => RegPlus.apply _ }
  def op = ((closure | option | plus)*) ^^ {
    case opList => (re: RegExp) => opList.foldLeft(re)((acc, oper) => oper(acc))
  }
  def unaryOp = (atom ~ op) ^^ { case re ~ oper => oper(re) }
  def concat = ((unaryOp)+) ^^ {
    case list if list.size == 1 => list(0)
    case list => RegConcat(list)
  }
  /*def orBody = (concat?) ^^ {
    case None => RegEmpty()
    case Some(v) => v
  }*/
  //here, we allow ors to be split into new lines
  def regExp: Parser[RegExp] = (concat ~ (("""\s*\|""".r ~> concat)*)) ^^ { 
    case first ~ rest if rest.isEmpty => first
    case first ~ rest => RegOr(first +: rest)
  }
  
  def rule = nontermRegId ~ ("->" | "::=") ~ regExp ^^ {
    case leftSide ~ arrow ~ rightSide => {
      //println("Lside: "+leftSide+" rightSide: "+rightSide)
      BNFRule(leftSide, rightSide)
    }
  }

  //separator for rules: new line followed by any sequence of white spaces
  def rules = (rule ~ (("""\n\s*""".r ~> rule)*)) ^^ {
    case rule ~ rules => rule +: rules 
  }  

  /* def rightsides = (("|" ~> regExp)*)
  def compositeRule = rule ~ rightsides ^^ { case r ~ rls => r +: rls.map(rs => (r._1, rs)) }*/

  /*def ruleStringsToRules(ruleStrings: List[(String, RegExp): List[BNFRule] = {
    //make the left side of rule strings nonterminals
    val nonterminals = ruleStrings.map(rs => rs._1)
    val rules = ruleStrings.map(ruleString => {
      val lhs = Nonterminal(ruleString._1)
      val rhs = ruleString._2.map(_.fold(re => Left(re), string => {
          val symbol = string match {
            case s if nonterminals.contains(s) => Nonterminal(s)
            case s => Terminal(s)
          }
          Right(symbol)
        }))       
      BNFRule(lhs, rhs)
    })
    rules
  }*/

  /*def rules = ((compositeRule <~ """\n|\t""")*) ^^ { _.flatten }
  def parseRules(input: String) = {
    parseAll(rules, input) match {
      case Success(ruleStrings, _) => ruleStringsToRules(ruleStrings)
      case failure => throw new IllegalArgumentException("Parsing Rules failed: " + failure)
    }
  }*/

  /**
   * Parses a sequence of lines corresponding to BNF rules of a grammar
   */
  def parseRules(lines: List[String]) = {
    val linesStr = lines.mkString("\n")
    //println("Lines: "+linesStr)
    parseAll(rules, linesStr) match {
      case Success(bnfRules, _) =>
        /*if (bnfRules.isEmpty)
          (bnfRules, "Input is empty!")
        else*/
          (bnfRules, "")
      case NoSuccess(err, input) =>
        val errstr = "failed to parse input " +
          "(line " + input.pos.line + ", column " + input.pos.column + "):\n" +
          err + "\n" +
          input.pos.longString
        (List(), errstr)
    }
  }

  /*def parseRules(lines: List[String]) = {

    var rulenum = 0
    var errstr = "";
    val rules = lines.foldLeft(List[BNFRule]())((acc, line) => {
      rulenum = rulenum + 1
      if (!errstr.isEmpty)
        acc
      else if (line.trim().isEmpty())
        //ignore this line and goto the next one
        acc
      else {
        parseAll(rule, line) match {
          case Success(bnfRule, _) =>
            acc :+ bnfRule
          case NoSuccess(err, input) =>
            errstr += ("failed to parse input " +
              "(line " + rulenum + ", column " + input.pos.column + "):\n" +
              err + "\n" +
              input.pos.longString)
            acc
        }
      }
    })
    if (errstr.isEmpty() && rules.isEmpty)
      (List(), "Input is empty!")
    else
      (rules, errstr)
  }*/
  
  def parseGrammarContent(file: String) = {
    parseGrammar(file.split("\n").toList)
  }

  /**
   * Parses a grammar consisting of a set of production rules given as a sequence of lines
   * Note:
   * a) The nonterminal in the left side of the first production rule is assumed to be the start symbol
   * b) The symbols that do not appear in the left side of the rules are considered as terminals
   */
  def parseGrammar(lines: List[String]) = {
    val (rules, errstr) = parseRules(lines)
    if (!errstr.isEmpty) {
      (None, errstr)
    } else {
      //start symbol is the nonterminal in the first rule by convention
      val startSymbol = rules(0).leftSide
      (Some(BNFGrammar[String](startSymbol, rules)), errstr)
    }
  }
}