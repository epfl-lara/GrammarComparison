package grammarcomp

package grammar
package utils

/**
 * A collection of methods for printing log messages
 */
object Logging {
  
  def printDebugMessage(key: String, msg: String) = {
    println(s"""[$key] $msg""")
  }
}