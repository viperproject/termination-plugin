package viper.termination.proofcode

import viper.silver.ast._
import viper.silver.verifier.AbstractError
import viper.termination.proofcode.util._
import viper.termination.{DecreasesExp, DecreasesStar}

class TerminationMethod(override val program: Program,
                        override val methodsDec: Map[String, DecreasesExp],
                        override val reportError: AbstractError => Unit)
  extends ProgramManager with MethodCheck {

  /**
    * Creates a new program with the additional features.
    * Should only be called once.
    *
    * @return new program.
    */
  override protected def createCheckProgram(): Program = {
    program.methods.filterNot(m => m.body.isEmpty || getMethodDecreasesExp(m.name).isInstanceOf[DecreasesStar]).foreach(m => {
      val context = MContext(m.name)

      val body: Stmt = methodStrategy(context).execute(m.body.get)

      // get all predicate init values which are used.
      val newVarPred = getMethodsInitPredLocVar(m.name)
      val newVarPredAss: Seq[Stmt] = newVarPred.map(v => generatePredicateAssign(v._2.localVar, v._1.loc)).toSeq

      val methodBody: Seqn = Seqn(newVarPredAss :+ body, newVarPred.values.toIndexedSeq)()
      val method = m.copy(body = Option(methodBody))(m.pos, m.info, m.errT)

      methods(m.name) = method
    })
    super.createCheckProgram()
  }

  case class MContext(override val methodName: String) extends ProofMethodContext
}