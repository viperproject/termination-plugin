package viper.termination.proofcode

import viper.silver.ast._
import viper.silver.verifier.AbstractError
import viper.termination.{DecreasesExp, DecreasesStar}
import viper.termination.proofcode.util._

class TerminationProof(override val program: Program,
                       override val functionsDec: Map[Function, DecreasesExp],
                       override val methodsDec: Map[String, DecreasesExp],
                       override val reportError: AbstractError => Unit)
  extends ProgramManager with MethodCheck with FunctionCheck {

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
      methods(m.name) = m.copy(body = Option(Seqn(Seq(body), Nil)()))(m.pos, m.info, m.errT)
    })

    program.functions.filterNot(f => f.body.isEmpty || getFunctionDecreasesExp(f).isInstanceOf[DecreasesStar]).foreach(f => {
      val methodName = uniqueName(f.name + "_termination_proof")
      val context = FContext(f, methodName)
      val toTransform = (f.body.get, context)
      val body = transformFuncBody(toTransform)

      // get all predicate init values which are used.
      val newVarPred = initPredLocVar.getOrElse(methodName, Map.empty)
      val newVarPredAss: Seq[Stmt] = newVarPred.map(v => generatePredicateAssign(v._2.localVar, v._1.loc)).toSeq

      val methodBody: Seqn = Seqn(newVarPredAss :+ body, newVarPred.values.toIndexedSeq)()
      val method = Method(methodName, f.formalArgs, Nil, f.pres, Nil, Option(methodBody))()

      methods(methodName) = method
    })

    super.createCheckProgram()
  }

  case class FContext(override val func: Function, override val methodName: String) extends FunctionContext
  case class MContext(override val methodName: String) extends ProofMethodContext
}