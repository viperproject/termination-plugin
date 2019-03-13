package viper.termination

import viper.silver.ast
import viper.silver.ast.{Method, Program, While}
import viper.termination.proofcode.{DecreasesExp, ProofDecreasesPath}

// run --plugin viper.silver.plugin.DecreasesPath [file]
class DecreasesPath extends DecreasesPlugin {

  override def transformToCheckProgram(input: Program, functionDecreasesMap: Map[ast.Function, DecreasesExp], methodDecreasesMap: Map[Method, DecreasesExp], whileDecreasesMap: Map[While, DecreasesExp]): Program = {
    val termCheck = new ProofDecreasesPath(input, functionDecreasesMap, reportError)
    termCheck.getNewProgram
  }
}