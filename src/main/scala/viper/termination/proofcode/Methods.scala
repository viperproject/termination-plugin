package viper.termination.proofcode

import org.jgrapht.alg.cycle.CycleDetector
import org.jgrapht.graph.{DefaultDirectedGraph, DefaultEdge}
import viper.silver.ast.{Method, MethodCall, Node, Program}

import scala.collection.JavaConverters._


object Methods {

  def allSubs(method: Method): Seq[Node] = method.pres ++ method.posts ++ method.body

  /** Returns the call graph of a given program (also considering specifications as calls).
    *
    * TODO: Memoize invocations of `getFunctionCallgraph`.
    */
  def getMethodCallgraph(program: Program, subs: Method => Seq[Node] = allSubs)
  : DefaultDirectedGraph[Method, DefaultEdge] = {

    val graph = new DefaultDirectedGraph[Method, DefaultEdge](classOf[DefaultEdge])

    for (m <- program.methods) {
      graph.addVertex(m)
    }

    def process(m: Method, n: Node) {
      n visit {
        case MethodCall(m2name, _, _) =>
          graph.addEdge(m, program.findMethod(m2name))
      }
    }

    for (m <- program.methods) {
      subs(m) foreach (process(m, _))
    }

    graph
  }

  def getMethodCluster(m: Method, program: Program): Set[Method] = {
    val graph = getMethodCallgraph(program)
    val cycleDetector = new CycleDetector[Method, DefaultEdge](graph)
    val cluster = cycleDetector.findCyclesContainingVertex(m).asScala
    cluster.add(m)
    cluster.toSet
  }
}