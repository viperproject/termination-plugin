
package viper.termination.trafo.util

import viper.silver.ast.utility.Functions
import viper.silver.ast.{ErrTrafo, Exp, FuncApp, Function, NodeTrafo, Seqn, Stmt}
import viper.silver.verifier.errors.AssertFailed
import viper.termination.{DecreasesExp, DecreasesTuple}

import scala.collection.immutable.ListMap

trait FunctionCheckPath extends ProgramManager with DecreasesCheck with FunctionTransformer {

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
    * Adds case FuncApp.
    * Inlines function calls (if the functions have the same height) until a loop is detected.
    * Then checks if the termination measure decreased     *
    * @return a statement containing all the inlining and the termination checks
    */
  override def transformFuncBody: PartialFunction[(Exp, Context), Stmt] = {
    case (callee: FuncApp, c: Context) =>
      // need Path context
      assert(c.isInstanceOf[PathContext], "Wrong context used for transformFuncBody")
      val context = c.asInstanceOf[PathContext]

      val func = context.func

      val stmts = collection.mutable.ArrayBuffer[Stmt]()

      // check the arguments
      val termChecksOfArgs: Seq[Stmt] = callee.getArgs map (a => transformFuncBody(a, context))
      stmts.appendAll(termChecksOfArgs)

      val calledFunc = functions(callee.funcname)
      val calleeArgs = callee.getArgs.map(transformExp(_, context))

      if (compareHeights(func, calledFunc)) {
        // In the same cycle
        val newFuncAppList = context.funcAppList :+ callee
        val newAlreadyChecked = context.alreadyChecked + callee.funcname

        // map of parameters in the called function to parameters in the current functions (for substitution)
        val mapFormalArgsToCalledArgs = ListMap(calledFunc.formalArgs.map(_.localVar).zip(calleeArgs): _*)

        if (!context.alreadyChecked.contains(callee.funcname)) {
          // not yet unrolled

          if (calledFunc.body.nonEmpty) {
            val body = calledFunc.body.get.replace(mapFormalArgsToCalledArgs)
            val newContext = context.copy(newFuncAppList, newAlreadyChecked)

            val unrolled = transformFuncBody(body, newContext)
            stmts.append(unrolled)

          } else {
            // function with an empty body in the same cycle?!
            assert(assertion = false, "Function with an empty body in the same cycle. Should not be possible!")
          }

        } else {
          // already unrolled => check termination measure

          val decOrigin = getFunctionDecreasesExp(func)
          val decDest = getFunctionDecreasesExp(calledFunc)

          assert(decOrigin.isInstanceOf[DecreasesTuple], "Checking a function with DecreasesStar for termination!" +
            "This should not happen!")

          val errTrafo = ErrTrafo({
            case AssertFailed(_, r, c) => TerminationFailed(newFuncAppList.head, r, c)
            case d => d
          })

          val reasonTrafoFactory = PathReasonTrafoFactory(decOrigin.asInstanceOf[DecreasesTuple], newFuncAppList)

          val terminationCheck = createTerminationCheck(decOrigin, decDest, mapFormalArgsToCalledArgs, errTrafo, reasonTrafoFactory, context)

          val assertion = terminationCheck

          stmts.appendAll(Seq(assertion))
        }
      }else{
        // not in the same cycle
      }
      Seqn(stmts, Nil)()

    case default => super.transformFuncBody(default)
  }
}

trait PathContext extends Context with FunctionContext{
  val funcAppList: Seq[FuncApp]
  val alreadyChecked: Set[String]
  def copy(newFuncAppList: Seq[FuncApp] = funcAppList,
           newAlreadyChecked: Set[String] = alreadyChecked): PathContext
}

