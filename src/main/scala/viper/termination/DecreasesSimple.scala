package viper.termination

import viper.silver.ast
import viper.silver.ast.{Program, While}
import viper.termination.proofcode.TerminationFunction

// run --plugin viper.silver.plugin.DecreasesSimple [file]
class DecreasesSimple extends DecreasesPlugin {
  override def transformToCheckProgram(input: Program, functionDecreasesMap: Map[ast.Function, DecreasesExp], methodDecreasesMap: Map[String, DecreasesExp], whileDecreasesMap: Map[While, DecreasesExp]): Program = {
    val termCheck = new TerminationFunction(input, functionDecreasesMap, reportError)
    termCheck.getNewProgram
  }
}