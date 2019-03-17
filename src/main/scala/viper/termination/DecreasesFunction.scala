package viper.termination

import viper.silver.ast
import viper.silver.ast.Program
import viper.termination.trafo.TrafoFunction

/**
  * Adds termination checks to functions.
  * run --plugin viper.termination.DecreasesFunction [file]
  */
class DecreasesFunction extends DecreasesPlugin {
  override def transformToCheckProgram(input: Program, functionDecreasesMap: Map[ast.Function, DecreasesExp], methodDecreasesMap: Map[String, DecreasesExp]): Program = {
    val termCheck = new TrafoFunction(input, functionDecreasesMap, reportError)
    termCheck.getNewProgram
  }
}