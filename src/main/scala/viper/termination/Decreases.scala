
package viper.termination

import viper.silver.ast
import viper.silver.ast.Program
import viper.termination.trafo.Trafo

/**
  * Adds termination checks to methods.
  *
  * Adds termination checks to functions.
  * (Follows execution path until a loop is reached)
  *
  * run --plugin viper.termination.Decreases [file]
  */
class Decreases extends DecreasesPlugin {
  override def transformToCheckProgram(input: Program, functionDecreasesMap: Map[ast.Function, DecreasesExp], methodDecreasesMap: Map[String, DecreasesExp]): Program = {
    val termCheck = new Trafo(input, functionDecreasesMap, methodDecreasesMap, reportError)
    termCheck.getNewProgram
  }
}