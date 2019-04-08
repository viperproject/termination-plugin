package viper.plugin.termination

import viper.silver.ast
import viper.silver.ast.Program
import viper.plugin.termination.trafo.Trafo
import viper.plugin.termination.trafo.util.FunctionCheck

/**
  * Adds termination checks to functions.
  * run --plugin viper.termination.DecreasesFunction [file]
  */
class DecreasesFunction extends AbstractDecreasesPlugin {
  override def transformToCheckProgram(input: Program, functionDecreasesMap: Map[ast.Function, DecreasesExp], methodDecreasesMap: Map[String, DecreasesExp]): Program = {
    val termCheck = new Trafo(input, reportError) with FunctionCheck {
      override val functionsDec: Map[ast.Function, DecreasesExp] = functionDecreasesMap
    }

    termCheck.getCheckProgram
  }
}