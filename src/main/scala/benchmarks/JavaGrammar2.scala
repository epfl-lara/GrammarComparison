package benchmarks
import grammar._
import grammar.CFGrammar._
import java.io._
import generators.GrammarBoundingHelper
import grammar.CNFConverter._

//source: 
object OracleJavaGrammar extends Benchmark {
  import GrammarReaders._  

  def benchmarkName = "OracleJavaGrammar"
  def benchmarkSource = "http://docs.oracle.com/javase/specs/jls/se7/html/jls-18.html"
    
  //val filename = "grammar-import/oracle-java-grammar.gram" //old
  val filename = "grammar-import/oracle-java-grammar-nlrec.gram"
  def ebnfGrammar = {
    GrammarReaders.readFromFile(filename)
  }    

  val initFilename = "grammar-import/Oracle-Java-Syntax.txt"
  def main(args: Array[String]) = {
    //parse the file initFilename and convert it to the ebnf form
    val fis = new FileInputStream(new File(initFilename))
    val br = new BufferedReader(new InputStreamReader(fis))
    var break = false
    var gramstr = ""
    while (!break) {
      val lineOrNull = br.readLine()
      //println("Processing line: " + lineOrNull)
      if (lineOrNull == null) {
        //reached EOF
        break = true
      } else {
        val line = lineOrNull.trim()
        if (line.isEmpty()) {
          //do nothing, simply skip
        } else {
          val lhs = line.split(":")(0)
          var break_inner = false
          var rhs = List[String]()
          while (!break_inner) {
            val line = br.readLine()
            if (line == null) {
              break_inner = true
              break = true
            } else {
              val rightside = line.trim()
              if (rightside.isEmpty())
                break_inner = true
              else {
                rhs :+= convertToEBNF(rightside)
              }
            }
          }
          val rule = lhs + " -> " + rhs.mkString(" | ")
          gramstr += rule + "\n"
          println("Rule: " + rule)
        }
      }
    }
    br.close()
    fis.close()

    //use gram str to construct grammar
    val pw = new PrintWriter(new FileOutputStream(new File(filename)))
    pw.print(gramstr)
    pw.close()
  }

  def specialChar(c: Char): Boolean = {
    //if (c == '{' || c == '[' || c == '(')
    if (c == '{' || c == '[')
      true
    else
      false
  }

  def isEndSpecialChar(c: Char): Boolean = {
    //if (c == '}' || c == ']' || c == ')')
    if (c == '}' || c == ']')
      true
    else
      false
  }

  def getEndSpecialChar(c: Char): Option[Char] = {
    if (c == '{')
      Some('}')
    else if (c == '[')
      Some(']')
    //    else if (c == '(')
    //      Some(')')
    else
      None
  }

  def getEBNFString(op: Char, str: String): String = {
    if (op == '{') {
      //zero or more occurences
      "(" + str + ")*"
    } else if (op == '[') {
      //zero or one occurrence
      "(" + str + ")?"
    } else if (op == '(') {
      //nothing else is required here
      "(" + str + ")"
    } else
      throw new IllegalStateException("Unkonwn Operation: " + op)
  }

  def isReservedChar(c: Char): Boolean = {
    //(c == '(' || c == ')' || c == '?' || c == '*' | c == '|' | c == '+')
    (c == '?' || c == '*' | c == '|' | c == '+')
  }

  def convertToEBNF(rside: String): String = {
    var resString = ""
    var i = 0
    while (i < rside.length()) {
      val begchar = rside(i)
      if (specialChar(begchar)) {
        val endchar = getEndSpecialChar(begchar).get
        var j = i + 1
        var break = false
        var nestedSpecChar = 0
        var modstr = ""

        while (!break) {
          if (j < rside.length()) {
            val jchar = rside(j)
            if (jchar == endchar && nestedSpecChar == 0)
              break = true
            else {
              if (specialChar(jchar))
                nestedSpecChar += 1
              else if (isEndSpecialChar(jchar))
                nestedSpecChar -= 1

              j = j + 1
              //enclose every reserved character in str by single quotes   
              val newchar = if (isReservedChar(jchar)) "'" + jchar + "'"
              else jchar
              modstr += newchar
            }
          } else
            break = true
        }

        val ebnfstring = getEBNFString(begchar, modstr)
        resString += ebnfstring
        i = j + 1
      } else {
        //here do no conversion
        val newchar = if (isReservedChar(begchar)) "'" + begchar + "'"
        else begchar
        resString += newchar
        i = i + 1
      }
    }
    resString
  }  
  //val boundFilename = "oraclegrammar-bounded"  
  //lazy val boundedGrammar =  GrammarBoundingHelper.createBoundedGrammar(javaGrammar,Some(1))
}