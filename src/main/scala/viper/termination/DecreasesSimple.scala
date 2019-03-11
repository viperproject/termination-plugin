package viper.termination

import viper.silver.ast
import viper.silver.ast.Program
import viper.termination.proofcode.{CheckDecreasesSimple, DecreasesExp}

// run --plugin viper.silver.plugin.DecreasesSimple [file]
class DecreasesSimple extends DecreasesPlugin {
  override def transformToCheckProgram(input: Program, decreasesMap: Map[ast.Function, DecreasesExp]): Program = {
    val termCheck = new CheckDecreasesSimple(input, decreasesMap, reportError)
    termCheck.getNewProgram
  }
}