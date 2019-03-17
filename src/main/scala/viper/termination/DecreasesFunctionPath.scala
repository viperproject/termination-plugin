package viper.termination

import viper.silver.ast
import viper.silver.ast.Program
import viper.termination.trafo.TrafoFunctionPath

/**
  * Adds termination checks to functions.
  * (Follows execution path until a loop is reached)
  * run --plugin viper.termination.DecreasesFunctionPath [file]
  */
class DecreasesFunctionPath extends DecreasesPlugin {
  override def transformToCheckProgram(input: Program, functionDecreasesMap: Map[ast.Function, DecreasesExp], methodDecreasesMap: Map[String, DecreasesExp]): Program = {
    val termCheck = new TrafoFunctionPath(input, functionDecreasesMap, reportError)
    termCheck.getNewProgram
  }
}