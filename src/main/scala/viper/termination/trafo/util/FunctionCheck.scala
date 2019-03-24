
package viper.termination.trafo.util

import viper.silver.ast.utility.{Functions, ViperStrategy}
import viper.silver.ast.utility.Rewriter.Traverse
import viper.silver.ast.{ErrTrafo, Exp, FuncApp, Function, LocalVar, LocalVarDecl, Method, NodeTrafo, Program, Result, Seqn, Stmt}
import viper.silver.verifier.errors.AssertFailed
import viper.termination.{DecreasesExp, DecreasesStar, DecreasesTuple}

import scala.collection.immutable.ListMap

trait FunctionCheck extends CheckProgramManager with DecreasesCheck with FunctionTransformer {

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

  protected override def generateCheckProgram(): Program = {
    program.functions.filterNot(f => f.body.isEmpty || getFunctionDecreasesExp(f).isInstanceOf[DecreasesStar]).foreach(f => {
      val methodName = uniqueName(f.name + "_termination_proof")
      val context = FContext(f, methodName)

      val resultVariableName = "$result"
      val resultVariable = LocalVarDecl(resultVariableName, f.typ)(f.result.pos, f.result.info, NodeTrafo(f.result))

      // TODO: check posts in another function and assume already checked postconditions
      val posts = f.posts.map(p => ViperStrategy.Slim({
        case r@Result() => LocalVar(resultVariableName)(r.typ, r.pos, r.info, NodeTrafo(r))
      }, Traverse.BottomUp).execute[Exp](p))

      val postsCheck = posts.map(transformFuncBody(_, context))
      val bodyCheck = transformFuncBody(f.body.get, context)

      // get all predicate init values which are used.
      val newVarPred = getMethodsInitPredLocVar(methodName)
      val newVarPredAss: Seq[Stmt] = newVarPred.map(v => generatePredicateAssign(v._2.localVar, v._1.loc)).toSeq

      val methodBody: Seqn = Seqn(newVarPredAss ++ postsCheck :+ bodyCheck, newVarPred.values.toIndexedSeq)()
      val method = Method(methodName, f.formalArgs, Seq(resultVariable), f.pres, Nil, Option(methodBody))()

      methods(methodName) = method
    })

    super.generateCheckProgram()
  }

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
  case class FContext(override val func: Function, override val methodName: String) extends FunctionContext

}

trait FunctionContext extends Context with ProofMethodContext {
  val func: Function
}

