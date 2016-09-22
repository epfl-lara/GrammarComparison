package grammarcomp

package grammar
package utils

import scala.annotation.tailrec

/**
 * A collection of helper methods.
 */
object GraphUtil {

  abstract class DirectedGraph[V] {
    def start: V
    def vertices: List[V]
    //def edges : Set[E]
    def successors(v: V): List[V]
    //add more interfaces here if necessary 

    type Component = List[V]

    case class State(count: Int,
      visited: Map[V, Boolean],
      dfNumber: Map[V, Int],
      lowlinks: Map[V, Int],
      stack: List[V],
      components: List[Component])

    val initial = State(
      count = 1,
      visited = vertices.map{ (_, false) }.toMap,
      dfNumber = Map(),
      lowlinks = Map(),
      stack = Nil,
      components = Nil)

    def sccs: List[Component] = {

      var state = search(start, initial)

      while (state.visited.exists(_._2 == false)) {
        state.visited.find(_._2 == false).foreach { tuple =>
          val (vertex, _) = tuple
          state = search(vertex, state)
        }
      }
      state.components
    }

    def search(vertex: V, state: State): State = {

      val newState = state.copy(visited = state.visited.updated(vertex, true),
        dfNumber = state.dfNumber.updated(vertex, state.count),
        count = state.count + 1,
        lowlinks = state.lowlinks.updated(vertex, state.count),
        stack = vertex :: state.stack)

      def processVertex(st: State, w: V): State = {
        if (!st.visited(w)) {
          val st1 = search(w, st)
          val min = Math.min(st1.lowlinks(w), st1.lowlinks(vertex))
          st1.copy(lowlinks = st1.lowlinks.updated(vertex, min))
        } else {
          if ((st.dfNumber(w) < st.dfNumber(vertex)) && st.stack.contains(w)) {
            val min = Math.min(st.dfNumber(w), st.lowlinks(vertex))
            st.copy(lowlinks = st.lowlinks.updated(vertex, min))
          } else st
        }
      }

      val strslt = successors(vertex).foldLeft(newState)(processVertex)

      if (strslt.lowlinks(vertex) == strslt.dfNumber(vertex)) {

        val index = strslt.stack.indexOf(vertex)
        val (comp, rest) = strslt.stack.splitAt(index + 1)
        strslt.copy(stack = rest,
          components = strslt.components :+ comp)
      } else strslt
    }
  }   
}