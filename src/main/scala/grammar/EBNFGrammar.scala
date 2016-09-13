package grammar


object EBNFGrammar {
  import CFGrammar._
  import BNFConverter._
  
  /**
   * A grammar in Backus-Naur form
   */ 
  sealed abstract class RegExp {
    // operators for the DSL
    def |(other: RegExp): RegExp = RegOr(List(this, other))
    def ~(other: RegExp) : RegExp = RegConcat(List(this, other))
    def * : RegExp = RegClosure(this)
    def + : RegExp = RegPlus(this)
    def ? : RegExp = RegOption(this)
  }
  case class RegOr(args: List[RegExp]) extends RegExp {
    override def toString = args.mkString(" | ")
  }
  case class RegConcat(args: List[RegExp]) extends RegExp {
    override def toString = (args map {
      case or @ RegOr(_) => "(" + or.toString + ")"
      case other@_ => other.toString 
    }).mkString(" ") 
  }
  case class RegPlus(re: RegExp) extends RegExp {
    override def toString = re match {
      case RegOr(_) | RegConcat(_) =>
        "("+re.toString()+")+"
      case _ =>
        re.toString()+"+"
    }
  }
  case class RegClosure(re: RegExp) extends RegExp {
    override def toString = re match {
      case RegOr(_) | RegConcat(_) =>
        "("+re.toString()+")*"
      case _ =>
        re.toString()+"*"
    }
  }
  case class RegOption(re: RegExp) extends RegExp {
    override def toString = re match {
      case RegOr(_) | RegConcat(_) =>
        "("+re.toString()+")?"
      case _ =>
        re.toString()+"?"
    }
  }   
  
  /**
   * A generic identifier class, for  allowing the content to be anything.
   * This may or may not represent non-terminals.
   * We are loosing typing here for programming convenience. 
   * But, this loss of typing is not visible at the user level.
   * This classes uses value equality and hence behaves similar to a case class
   */
  class GenericRegId(val obj: Any) extends RegExp {
    override def hashCode = obj.hashCode()
    override def equals(other: Any) = other match {
      case rid : GenericRegId => rid.obj  == obj
      case _ => false
    }
    override def toString = obj match {
      case s : String =>
        if (s.matches("""([^\s\|\*\+\(\)\?'])+""")) s
        else "'" + s + "'"
      case _ => obj.toString()
    }
  }
  
  /**
   * A class that represents non-terminals appearing on the LHS of rules.
   * The RHS non-terminals may be genericRegId or nontermId
   * This allows distinguishing between terminals and non-terminals
   */
  class NontermId(val name: String) extends GenericRegId(name) {
    // a DSL construct for creating a rule
    def -> (rhs: RegExp) = BNFRule(this, rhs)           
    
    override def toString = name
  }
  case class RegEmpty() extends RegExp {
    override def toString = "\"\""      
  }
      
  case class BNFRule(leftSide: NontermId, rightSide: RegExp) {    
    override def toString = leftSide + " -> " + rightSide.toString()    
  }
  
  /**
   * The type parameter represents the type of the terminals
   */
  case class BNFGrammar[T](start: NontermId, rules : List[BNFRule]) {
    override def toString = rules.mkString("\n")
    
    def toHTMLString = {      
       rules.mkString("<br/>","<br/>","<br/>")
    }
    
    lazy val cfGrammar = ebnfToGrammar(this)
  }  
  
  /**
   * Exceptions for error handling
   */
  class InvalidGrammarException(msg: String) extends Exception(msg)
}