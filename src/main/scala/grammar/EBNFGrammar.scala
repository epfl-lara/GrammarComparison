package grammar


object EBNFGrammar {
  import CFGrammar._
  import BNFConverter._
    
  /**
   * A grammar in Backus-Naur form
   */ 
  sealed abstract class RegExp[+T] {
    // operators for the DSL
    /*def |[W >: T, U <: W](other: RegExp[U]): RegExp[W] = RegOr[W]( this :: (List(other) : List[RegExp[W]]) )
    def ~[W >: T, U <: W](other: RegExp[U]): RegExp[W] = RegConcat[W]( this :: (List(other) : List[RegExp[W]]) )    
    def * : RegExp[T] = RegClosure(this)
    def + : RegExp[T] = RegPlus(this)
    def ? : RegExp[T] = RegOption(this)*/
  }
  case class RegOr[T](args: List[RegExp[T]]) extends RegExp[T] {
    override def toString = args.mkString(" | ")
  }
  case class RegConcat[T](args: List[RegExp[T]]) extends RegExp[T] {
    override def toString = (args map {
      case or @ RegOr(_) => "(" + or.toString + ")"
      case other@_ => other.toString 
    }).mkString(" ") 
  }
  case class RegPlus[T](re: RegExp[T]) extends RegExp[T] {
    override def toString = re match {
      case RegOr(_) | RegConcat(_) =>
        "("+re.toString()+")+"
      case _ =>
        re.toString()+"+"
    }
  }
  case class RegClosure[T](re: RegExp[T]) extends RegExp[T] {
    override def toString = re match {
      case RegOr(_) | RegConcat(_) =>
        "("+re.toString()+")*"
      case _ =>
        re.toString()+"*"
    }
  }
  case class RegOption[T](re: RegExp[T]) extends RegExp[T] {
    override def toString = re match {
      case RegOr(_) | RegConcat(_) =>
        "("+re.toString()+")?"
      case _ =>
        re.toString()+"?"
    }
  }   
  
  /**
   * A class that represents terminals or non-terminals, if we do not know which one is what.
   * This happens when a grammar is read as input, and not constructed using DSL 
   */
  class Sym[+T](val obj: Any) extends RegExp[T] {
    override def hashCode = obj.hashCode()
    override def equals(other: Any) = other match {
      case sym : Sym[T] => sym.obj == obj
      case _ => false
    }
  }  
  
  class Term[T](val content: T) extends Sym[T](content) {    
    override def toString = content match {
      case s : String =>
        if (s.matches("""([^\s\|\*\+\(\)\?'])+""")) s
        else "'" + s + "'"
      case _ => content.toString()
    }
  }
  
  /**
   * A class that represents non-terminals appearing on the LHS of rules.   
   */
  class NontermId(val sym: scala.Symbol) extends Sym[Nothing](sym) {
    // a DSL construct for creating a rule
    //def ->[T] (rhs: RegExp[T]) = BNFRule(this, rhs)                  
    override def toString = sym.toString
  }
  
  case class RegEmpty() extends RegExp {
    override def toString = "\"\""      
  }
      
  case class BNFRule[T](leftSide: NontermId, rightSide: RegExp[T]) {    
    override def toString = leftSide + " -> " + rightSide.toString()    
  }
  
  /**
   * The type parameter represents the type of the terminals
   */
  case class BNFGrammar[T](start: NontermId, rules : List[BNFRule[T]]) {
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