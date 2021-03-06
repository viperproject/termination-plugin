// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2019 ETH Zurich.

package viper.plugin.termination.trafo.util

import org.jgrapht.alg.cycle.CycleDetector
import org.jgrapht.graph.{DefaultDirectedGraph, DefaultEdge}
import viper.silver.ast.{Method, MethodCall, Node, Program}

import scala.collection.JavaConverters._

/**
  * Utility methods for Methods.
  */
object Methods {

  def allSubs(method: Method): Seq[Node] = method.pres ++ method.posts ++ method.body

  /** Returns the call graph of a given program.
    *
    * TODO: Memoize invocations of `getFunctionCallgraph`.
    */
  def getMethodCallgraph(program: Program, subs: Method => Seq[Node] = _.body.getOrElse(Seq()).toSeq)
                        : DefaultDirectedGraph[Method, DefaultEdge] = {

    val graph = new DefaultDirectedGraph[Method, DefaultEdge](classOf[DefaultEdge])

    program.methods.foreach(graph.addVertex)

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

  /**
    * @param m: method
    * @param program containing the methods
    * @return set of methods which call each other recursively (including m) in the program
    */
  def getMethodCluster(m: Method, program: Program): Set[Method] = {
    val graph = getMethodCallgraph(program)
    val cycleDetector = new CycleDetector(graph)
    val cluster = cycleDetector.findCyclesContainingVertex(m).asScala
    cluster.add(m)
    cluster.toSet
  }
}
