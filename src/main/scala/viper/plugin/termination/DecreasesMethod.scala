
package viper.plugin.termination

import viper.silver.ast
import viper.silver.ast.Program
import viper.plugin.termination.trafo.Trafo
import viper.plugin.termination.trafo.util.{FunctionCheckPath, MethodCheck}

/**
  * Adds termination checks to methods.
  * run --plugin viper.termination.DecreasesMethod [file]
  */
class DecreasesMethod extends DecreasesPlugin {
  override def transformToCheckProgram(input: Program, functionDecreasesMap: Map[ast.Function, DecreasesExp], methodDecreasesMap: Map[String, DecreasesExp]): Program = {
    val termCheck = new Trafo(input, reportError) with MethodCheck{
      override val methodsDec: Map[String, DecreasesExp] = methodDecreasesMap
    }
    termCheck.getCheckProgram
  }
}