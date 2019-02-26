package viper.termination

import viper.silver.ast
import viper.silver.ast.Program
import viper.termination.proofcode.{CheckDecreasesSimple, DecreasesExp}


class DecreasesSimple extends DecreasesPlugin {
  override def transformToCheckProgram(input: Program, decreasesMap: Map[ast.Function, DecreasesExp]): Program = {
    val termCheck = new CheckDecreasesSimple(input, decreasesMap)
    termCheck.createCheckProgram()
  }
}