
package viper.termination

import viper.silver.ast
import viper.silver.ast.{Program, While}
import viper.termination.proofcode.TerminationMethod

// run --plugin viper.silver.plugin.DecreasesSimple [file]
class DecreasesMethod extends DecreasesPlugin {
  override def transformToCheckProgram(input: Program, functionDecreasesMap: Map[ast.Function, DecreasesExp], methodDecreasesMap: Map[String, DecreasesExp], whileDecreasesMap: Map[While, DecreasesExp]): Program = {
    val termCheck = new TerminationMethod(input, methodDecreasesMap, reportError)
    termCheck.getNewProgram
  }
}