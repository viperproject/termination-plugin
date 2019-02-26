
package viper.termination

import viper.silver.ast
import viper.silver.ast.Program
import viper.termination.proofcode.{CheckDecreasesPlus, DecreasesExp}


class DecreasesPath extends DecreasesPlugin {
  override def transformToCheckProgram(input: Program, decreasesMap: Map[ast.Function, DecreasesExp]): Program = {
    val termCheck = new CheckDecreasesPlus(input, decreasesMap)
    termCheck.createCheckProgram()
  }
}