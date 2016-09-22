package grammarcomp

package engine

import grammar.utils._
import grammar._
import java.io._
import java.lang.management._
import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer
import grammar.EBNFGrammar._

object AntlrToGrammar {

  def main(args: Array[String]) {
    val filename = args(0)
    val fis = new FileInputStream(new File(filename))
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
        //drop every thing after '#'
        val commentIndex = lineOrNull.indexOf('#')
        val line = if (commentIndex >= 0)
          lineOrNull.take(commentIndex).trim()
        else
          lineOrNull.trim()
        if (line.isEmpty() || line.startsWith("//")) {
          //do nothing, simply skip
        } else {
          var break_inner = false
          val firstColon = line.indexOf(':') //index of the first colon
          val (lhs, part2) = if (firstColon == -1) {
            (line, "")
          } else {
            //ignore the colon when splitting
            (line.take(firstColon), line.drop(firstColon + 1))
          }
          var rhs = if (!part2.isEmpty() && part2.last == ';') {
            break_inner = true
            part2.dropRight(1)
          } else
            part2

          while (!break_inner) {
            val line = br.readLine()
            if (line == null) {
              break_inner = true
              break = true
            } else {
              val commentIndex = line.indexOf('#')
              val rightside = if (commentIndex >= 0) {
                line.take(commentIndex).trim()
              } else
                line.trim()
              //val rightside = line.trim()
              if (rightside.isEmpty()) {
                //simply skip                
              } //              else if (rightside == ";")
              //                break_inner = true
              else {
                //remove the first ':'
                val rexpr = if (rightside(0) == ':')
                  rightside.drop(1)
                else
                  rightside
                val ruleRHS =
                  if (rexpr.isEmpty())
                    "\"\""                  
                  else if (rexpr.last == ';') {
                    break_inner = true
                    rexpr.dropRight(1)
                  } else
                    rexpr
                rhs += " " + ruleRHS
              }
            }
          }
          val rule = lhs + " -> " + rhs
          gramstr += rule + "\n"
          println("Rule: " + rule)
        }
      }
    }
    br.close()
    fis.close()

    //use gram str to construct grammar
    val pw = new PrintWriter(new FileOutputStream(new File(filename + ".gram")))
    pw.print(gramstr)
    pw.close()
  }

  /*def convertToEBNF(rside: String): String = {
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
  }*/
}