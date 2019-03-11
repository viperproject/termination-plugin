package viper.termination

import viper.silver.ast
import viper.silver.ast.Program
import viper.termination.proofcode.{ProofDecreasesPath, DecreasesExp}

// run --plugin viper.silver.plugin.DecreasesPath [file]
class DecreasesPath extends DecreasesPlugin {

  override def transformToCheckProgram(input: Program, decreasesMap: Map[ast.Function, DecreasesExp]): Program = {
    val termCheck = new ProofDecreasesPath(input, decreasesMap, reportError)
    termCheck.getNewProgram
  }
}