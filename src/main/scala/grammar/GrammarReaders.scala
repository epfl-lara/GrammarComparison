package grammar

import CFGrammar._
import EBNFGrammar._
import scala.io.StdIn

object GrammarReaders {

  /**
   * Reads a grammar from the console
   */
  def readFromConsole: BNFGrammar[String] = {
    println("Enter Rules in the format given by the regex: ([a-zA-Z] -> [a-zA-Z]+ \\n)+")
    val lines = Iterator.continually(StdIn.readLine).takeWhile(_ != "").toList
    val (bnfG, errstr) = (new GrammarParser()).parseGrammar(lines)
    if (bnfG.isDefined)
      bnfG.get
    else
      throw new IllegalArgumentException(errstr)
  }
  
  def readFromFile(filename: String) : BNFGrammar[String] = {
    val lines = scala.io.Source.fromFile(filename).getLines.toList
    val (bnfG, errstr) = (new GrammarParser()).parseGrammar(lines)
    if (bnfG.isDefined)
      bnfG.get
    else
      throw new IllegalArgumentException(errstr)
  }
  
  def readDerivation(g: Grammar[String]): List[SententialForm[String]] = {
    println("Enter derivation steps")
    val lines = Iterator.continually(StdIn.readLine).takeWhile(_ != "").toList
    val (dervs, errstr) = (new SententialFormParser()).parseSententialForms(lines, g)
    if (errstr.isEmpty())
      dervs
    else
      throw new IllegalArgumentException(errstr)
  }

  /**
   * So that grammars in the code can be written grammar"""<rules>"""
   */
  implicit class GrammarImplicit(sc: StringContext) {
    val parser = new GrammarParser()

    def bnfgrammar(args: Any*): BNFGrammar[String] = {
      val string = sc.s(args: _*)
      val (bnfG, errstr) = parser.parseGrammar(string.split("\r?\n").toList)
      if (bnfG.isDefined)
        bnfG.get
      else
        throw new IllegalArgumentException(errstr)
    }

    def grammar(args: Any*): Grammar[String] = {
      bnfgrammar(args: _*).cfGrammar
    }

    /*def bnfrule(args: Any*): BNFRule = {
      val string = sc.s(args: _*)
      parser.parseRules(List(string)).head
    }*/
  }
}