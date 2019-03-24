
package viper.termination

import viper.silver.ast
import viper.silver.ast.Program
import viper.termination.trafo.Trafo
import viper.termination.trafo.util.{FunctionCheckPath, MethodCheck}

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
    val termCheck = new Trafo(input, reportError) with FunctionCheckPath with MethodCheck{
      override val functionsDec: Map[ast.Function, DecreasesExp] = functionDecreasesMap
      override val methodsDec: Map[String, DecreasesExp] = methodDecreasesMap
    }
    termCheck.getCheckProgram
  }
}