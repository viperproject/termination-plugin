package viper.termination

import viper.silver.ast
import viper.silver.ast.{Program, While}
import viper.termination.proofcode.TerminationFunctionPath

// run --plugin viper.silver.plugin.DecreasesPath [file]
class DecreasesFunctionPath extends DecreasesPlugin {

  override def transformToCheckProgram(input: Program, functionDecreasesMap: Map[ast.Function, DecreasesExp], methodDecreasesMap: Map[String, DecreasesExp], whileDecreasesMap: Map[While, DecreasesExp]): Program = {
    val termCheck = new TerminationFunctionPath(input, functionDecreasesMap, reportError)
    termCheck.getNewProgram
  }
}