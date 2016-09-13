package grammar


object EBNFGrammar {
  import CFGrammar._
  import BNFConverter._
  
  /**
   * A grammar in Backus-Naur form
   */ 
  sealed abstract class RegExp
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
   * We are loosing typing here for programming convenience. 
   * But, this loss of typing is not visible at the user level.
   * Uses value equality
   */
  class GenericRegId(val obj: Any) extends RegExp {
    override def hashCode = obj.hashCode()
    override def equals(other: Any) = other match {
      case rid : GenericRegId => rid.obj  == obj
      case _ => false
    }
    override def toString = obj.toString
  }
  
  /**
   * This is the class that would be created for non-terminals, and
   * terminals parsed from a string. This just does some pretty printing.
   */
  class RegId(name: String) extends GenericRegId(name) {     
    override def toString = {
      if(name.matches("""([^\s\|\*\+\(\)\?'])+""")) name
      else "'"+name+"'"
    }     
  }
  case class RegEmpty() extends RegExp {
    override def toString = "\"\""      
  }
      
  case class BNFRule(leftSide: GenericRegId, rightSide: RegExp) {    
    override def toString = leftSide + " -> " + rightSide.toString()    
  }
  
  /**
   * The type parameter represents the type of the terminals
   */
  case class BNFGrammar[T](start: GenericRegId, rules : List[BNFRule]) {
    override def toString = rules.mkString("\n")
    
    def toHTMLString = {      
       rules.mkString("<br/>","<br/>","<br/>")
    }
    
    lazy val cfGrammar = ebnfToGrammar(this)
  }  
}