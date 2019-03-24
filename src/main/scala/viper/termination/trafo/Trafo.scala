package viper.termination.trafo

import viper.silver.ast._
import viper.silver.verifier.AbstractError
import viper.termination.trafo.util._

class Trafo(override val program: Program,
            override val reportError: AbstractError => Unit)
  extends CheckProgramManager