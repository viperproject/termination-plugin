// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2019 ETH Zurich.

package viper.plugin.termination

import viper.silver.ast
import viper.silver.ast.Program
import viper.plugin.termination.trafo.Trafo
import viper.plugin.termination.trafo.util.FunctionCheck

/**
  * Adds termination checks to functions.
  * run --plugin viper.termination.DecreasesFunction [file]
  */
class DecreasesFunction extends AbstractDecreasesPlugin {
  override def transformToCheckProgram(input: Program, functionDecreasesMap: Map[ast.Function, DecreasesExp], methodDecreasesMap: Map[String, DecreasesExp]): Program = {
    val termCheck = new Trafo(input, reportError) with FunctionCheck {
      override val functionsDec: Map[ast.Function, DecreasesExp] = functionDecreasesMap
    }

    termCheck.getCheckProgram
  }
}