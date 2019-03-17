
package viper.termination

import viper.silver.ast
import viper.silver.ast.Program
import viper.termination.trafo.TrafoMethod

/**
  * Adds termination checks to methods.
  * run --plugin viper.termination.DecreasesMethod [file]
  */
class DecreasesMethod extends DecreasesPlugin {
  override def transformToCheckProgram(input: Program, functionDecreasesMap: Map[ast.Function, DecreasesExp], methodDecreasesMap: Map[String, DecreasesExp]): Program = {
    val termCheck = new TrafoMethod(input, methodDecreasesMap, reportError)
    termCheck.getNewProgram
  }
}