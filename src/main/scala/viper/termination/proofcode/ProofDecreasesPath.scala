
package viper.termination.proofcode

import viper.silver.ast._
import viper.silver.ast.utility.Functions
import viper.silver.verifier.errors.AssertFailed
import viper.silver.verifier.{AbstractError, AbstractErrorReason, errors}
import viper.silver.verifier.reasons.AssertionFalse

import scala.collection.immutable.ListMap

class ProofDecreasesPath(val program: Program, val decreasesMap: Map[Function, DecreasesExp], val reportError: AbstractError => Unit) extends ProofDecreases[PathContext]{

  private val heights: Map[Function, Int] = Functions.heights(program)
  private def compareHeights(f1: Function, f2: Function): Boolean= {
    // guess heights are always positive
    heights.getOrElse(f1, -1) == heights.getOrElse(f2, -2)
  }

  /**
    * This function should be used to access all the DecreasesExp
    * @param function for which the decreases exp is defined
    * @return the defined DecreasesExp or a DecreasesTuple with the parameters as the arguments
    */
  def getDecreasesExp(function: Function): DecreasesExp = {
    decreasesMap.getOrElse(function, {
      DecreasesTuple(function.formalArgs.map(_.localVar), function.pos, NodeTrafo(function))
    })
  }

  override protected def createCheckProgram(): Program = {
    // add termination check methods for all function (without the ones with empty body and decreases star)
    program.functions.filterNot(f => f.body.isEmpty || getDecreasesExp(f).isInstanceOf[DecreasesStar]).foreach(f => {
      val methodName = uniqueName(f.name + "_termination_proof")
      val context = PathContext(f, methodName, Nil, Set.empty + f.name)
      val body = transform(f.body.get, context)
      val localVars = neededLocalVars.get(methodName) match {
        case Some(v) => v.values
        case None => Nil
      }

      val methodBody: Seqn = Seqn(Seq(body), localVars.toIndexedSeq)()
      val method = Method(methodName, f.formalArgs, Nil, f.pres, Nil, Option(methodBody))()

      methods(methodName) = method
    })

    super.createCheckProgram()
  }

  /**
    * Adds case FuncApp.
    * Inlines function calls (if the functions have the same height) until a loop is detected.
    * Then checks if the termination measure decreased     *
    * @return a statement containing all the inlining and the termination checks
    */
  override def transform: PartialFunction[(Exp, PathContext), Stmt] = {
    case (callee: FuncApp, context: PathContext) =>
      val func = context.func

      val stmts = collection.mutable.ArrayBuffer[Stmt]()

      // check the arguments
      val termChecksOfArgs: Seq[Stmt] = callee.getArgs map (a => transform(a, context))
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

            val newContext = context.copy(funcAppList = newFuncAppList, alreadyChecked = newAlreadyChecked)

            val unrolled = transform(body, newContext)
            stmts.append(unrolled)

          } else {
            // function with an empty body in the same cycle?!
            assert(assertion = false, "Function with an empty body in the same cycle. Should not be possible!")
          }

        } else {
          // already unrolled => check termination measure

          val decOrigin = getDecreasesExp(func)
          val decDest = getDecreasesExp(calledFunc)

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

    case default => super.transform(default)
  }

  case class PathReasonTrafoFactory(offendingNode: DecreasesTuple, offendingPath: Seq[FuncApp]) extends ReasonTrafoFactory {
    override def createNotDecrease(biggerDec: Seq[Exp], smallerDec: Seq[Exp], context: Context): ReTrafo = {
      ReTrafo({ case AssertionFalse(_) => TerminationNoDecreasePath(offendingNode, biggerDec, smallerDec, offendingPath) })
    }

    override def createNotBounded(biggerDec: Seq[Exp], context: Context): ReTrafo = {
      ReTrafo({ case AssertionFalse(_) => TerminationNoBoundPath(offendingNode, biggerDec, offendingPath) })
    }

    override def createStar(context: Context): ReTrafo = {
      ReTrafo({ case AssertionFalse(_) => TerminationStarPath(offendingNode, offendingPath) })
    }
  }
}

case class PathContext(func: Function, methodName: String, funcAppList: Seq[FuncApp], alreadyChecked: Set[String]) extends FunctionContext

case class TerminationNoDecreasePath(offendingNode: DecreasesExp, decOrigin: Seq[Exp], decDest: Seq[Exp], offendingPath: Seq[FuncApp]) extends AbstractErrorReason {
  val id = "termination.no.decrease"
  override def readableMessage: String = s"Termination measure might not decrease. " +
    s"Assertion (${decDest.mkString(", ")})≺(${decOrigin.mkString(", ")}) might not hold. " +
    s"Path: ${getReadablePath(offendingPath)}."

  def getReadablePath(path: Seq[FuncApp]): String = {
    path.map(f => s"$f@${
      f.pos match {
        case NoPosition =>"noPos"
        case p: HasLineColumn => s"${p.line}.${p.column}"
      }}").mkString(" -> ")
  }

  def withNode(offendingNode: errors.ErrorNode = this.offendingNode) = TerminationNoDecreasePath(this.offendingNode, decOrigin, decDest, offendingPath)
}

case class TerminationNoBoundPath(offendingNode: DecreasesExp, decExp: Seq[Exp], offendingPath: Seq[FuncApp]) extends AbstractErrorReason {
  val id = "termination.no.bound"
  override def readableMessage: String = s"Termination measure might not be bounded. " +
    s"Assertion 0≺(${decExp.mkString(", ")}) might not hold. " +
    s"Path: ${getReadablePath(offendingPath)}."

  def getReadablePath(path: Seq[FuncApp]): String = {
    path.map(f => s"$f@${
      f.pos match {
        case NoPosition =>"noPos"
        case p: HasLineColumn => s"${p.line}.${p.column}"
      }}").mkString(" -> ")
  }

  def withNode(offendingNode: errors.ErrorNode = this.offendingNode) = TerminationNoBoundPath(this.offendingNode, decExp, offendingPath)
}

case class TerminationStarPath(offendingNode: DecreasesExp, offendingPath: Seq[FuncApp]) extends AbstractErrorReason {
  val id = "termination.star"
  override def readableMessage = s"Cannot prove termination, if a function with decreasesStar is called."

  def withNode(offendingNode: errors.ErrorNode = this.offendingNode) = TerminationStarPath(this.offendingNode, offendingPath)
}