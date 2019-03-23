
package viper.termination.trafo

import viper.silver.ast._
import viper.silver.ast.utility.Rewriter.Traverse
import viper.silver.ast.utility.ViperStrategy
import viper.silver.verifier.AbstractError
import viper.termination.trafo.util._
import viper.termination.{DecreasesExp, DecreasesStar}

class TrafoFunctionPath(override val program: Program,
                        override val functionsDec: Map[Function, DecreasesExp],
                        override val reportError: AbstractError => Unit)
  extends ProgramManager with FunctionCheckPath {

  /**
    * Creates a new program with the additional features.
    * Should only be called once.
    *
    * @return new program.
    */
  override protected def createCheckProgram(): Program = {
    program.functions.filterNot(f => f.body.isEmpty || getFunctionDecreasesExp(f).isInstanceOf[DecreasesStar]).foreach(f => {
      val methodName = uniqueName(f.name + "_termination_proof")
      val context = FContext(f, methodName, Nil, Set.empty + f.name)

      val resultVariableName = "$result"
      val resultVariable = LocalVarDecl(resultVariableName, f.typ)(f.result.pos, f.result.info, NodeTrafo(f.result))

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

    super.createCheckProgram()
  }

  case class FContext(override val func: Function,
                      override val methodName: String,
                      override val funcAppList: Seq[FuncApp],
                      override val alreadyChecked: Set[String]) extends PathContext {
    override def copy(newFuncAppList: Seq[FuncApp], newAlreadyChecked: Set[String]): PathContext = {
      FContext(func, methodName, newFuncAppList, newAlreadyChecked)
    }
  }
}