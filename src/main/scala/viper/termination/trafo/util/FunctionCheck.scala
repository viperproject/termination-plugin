
package viper.termination.trafo.util

import viper.silver.ast.utility.Functions
import viper.silver.ast.{ErrTrafo, Exp, FuncApp, Function, NodeTrafo, Seqn, Stmt}
import viper.silver.verifier.errors.AssertFailed
import viper.termination.{DecreasesExp, DecreasesTuple}

import scala.collection.immutable.ListMap

trait FunctionCheck extends ProgramManager with DecreasesCheck with FunctionTransformer {

  private val heights: Map[Function, Int] = Functions.heights(program)
  private def compareHeights(f1: Function, f2: Function): Boolean = {
    // guess heights are always positive
    heights.getOrElse(f1, -1) == heights.getOrElse(f2, -2)
  }

  /**
    * This function should be used to access all the DecreasesExp
    * @param function for which the decreases exp is defined
    * @return the defined DecreasesExp or a DecreasesTuple with the parameters as the arguments
    */
  def getFunctionDecreasesExp(function: Function): DecreasesExp = {
    functionsDec.getOrElse(function, {
      DecreasesTuple(function.formalArgs.map(_.localVar), function.pos, NodeTrafo(function))
    })
  }
  val functionsDec: Map[Function, DecreasesExp]


  /**
    * Adds case FuncApp
    * Checks if the termination measure decreases in every function call (to a possibly
    * recursive call)
    *
    * @return a statement representing the expression
    */
  override def transformFuncBody: PartialFunction[(Exp, Context), Stmt] = {
    case (callee: FuncApp, context: Context) =>
      assert(context.isInstanceOf[FunctionContext], "Wrong context used for transformFuncBody")
      val func = context.asInstanceOf[FunctionContext].func

      val stmts = collection.mutable.ArrayBuffer[Stmt]()

      // check the arguments
      val termChecksOfArgs: Seq[Stmt] = callee.getArgs map (a => transformFuncBody(a, context))
      stmts.appendAll(termChecksOfArgs)

      val calledFunc = functions(callee.funcname)
      val calleeArgs = callee.getArgs.map(transformExp(_, context))

      if (compareHeights(func, calledFunc)) {
        // In the same cycle. => compare

        // map of parameters in the called function to parameters in the current functions (for substitution)
        val mapFormalArgsToCalledArgs = ListMap(calledFunc.formalArgs.map(_.localVar).zip(calleeArgs): _*)

        val decOrigin = getFunctionDecreasesExp(func)
        val decDest = getFunctionDecreasesExp(calledFunc)

        assert(decOrigin.isInstanceOf[DecreasesTuple], "Checking a function with DecreasesStar for termination!" +
          "This should not happen!")

        val errTrafo = ErrTrafo({
          case AssertFailed(_, r, c) => TerminationFailed(callee, r, c)
          case d => d
        })

        val reasonTrafoFactory = SimpleReasonTrafoFactory(decOrigin.asInstanceOf[DecreasesTuple])

        val terminationCheck = createTerminationCheck(decOrigin, decDest, mapFormalArgsToCalledArgs, errTrafo, reasonTrafoFactory, context.asInstanceOf[ProofMethodContext])

        val assertion = terminationCheck

        stmts.appendAll(Seq(assertion))
      } else {
        // not in the same cycle
      }
      Seqn(stmts, Nil)()
    case default => super.transformFuncBody(default)
  }


}

trait FunctionContext extends Context with ProofMethodContext {
  val func: Function
}

