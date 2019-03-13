package viper.termination

import viper.silver.ast
import viper.silver.ast.{Method, Program, While}
import viper.termination.proofcode.{CheckDecreasesSimple, DecreasesExp}

// run --plugin viper.silver.plugin.DecreasesSimple [file]
class DecreasesSimple extends DecreasesPlugin {
  override def transformToCheckProgram(input: Program, functionDecreasesMap: Map[ast.Function, DecreasesExp], methodDecreasesMap: Map[Method, DecreasesExp], whileDecreasesMap: Map[While, DecreasesExp]): Program = {
    val termCheck = new CheckDecreasesSimple(input, functionDecreasesMap, reportError)
    termCheck.getNewProgram
  }
}