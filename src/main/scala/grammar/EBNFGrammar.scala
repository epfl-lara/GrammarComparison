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
  case class RegId(name: String) extends RegExp {
    override def toString = {
      if(name.matches("""([^\s\|\*\+\(\)\?'])+""")) name
      else "'"+name+"'"
    }     
  }
  case class RegEmpty() extends RegExp {
    override def toString = "\"\""      
  }
      
  case class BNFRule(leftSide: RegId, rightSide: RegExp) {    
    override def toString = leftSide + " -> " + rightSide.toString()    
  }
  
  case class BNFGrammar(start: RegId, rules : List[BNFRule]) {
    override def toString = rules.mkString("\n")
    
    def toHTMLString = {      
       rules.mkString("<br/>","<br/>","<br/>")
    }
    
    lazy val cfGrammar = ebnfToGrammar(this)
  }
}