
package viper.termination.proofcode

import viper.silver.ast._
import viper.silver.ast.utility.Functions
import viper.silver.verifier.errors.AssertFailed
import viper.silver.verifier.{AbstractError, AbstractErrorReason, errors}
import viper.silver.verifier.reasons.AssertionFalse

import scala.collection.immutable.ListMap

class CheckDecreasesSimple(val program: Program, val decreasesMap: Map[Function, DecreasesExp], val reportError: AbstractError => Unit) extends ProofDecreases[FunctionContext] {

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
    program.functions.filterNot(f => f.body.isEmpty || getDecreasesExp(f).isInstanceOf[DecreasesStar]).foreach(f => {
      val methodName = uniqueName(f.name + "_termination_proof")
      val context = SimpleContext(f, methodName)
      val body = transform(f.body.get, context)

      // get all predicate init values which are used.
      val newVarPred = initPredLocVar.getOrElse(methodName, Map.empty)
      val newVarPredAss: Seq[Stmt] = newVarPred.map(v => generatePredicateAssign(v._1.loc, v._1.perm, v._2.localVar)).toSeq

      val methodBody: Seqn = Seqn(newVarPredAss :+ body, newVarPred.values.toIndexedSeq)()
      val method = Method(methodName, f.formalArgs, Nil, f.pres, Nil, Option(methodBody))()

      methods(methodName) = method
    })

    super.createCheckProgram()
  }

  case class SimpleContext(func: Function, methodName: String) extends FunctionContext

  /**
    * Adds case FuncApp
    * Checks if the termination measure decreases in every function call (to a possibly
    * recursive call)
    *
    * @return a statement representing the expression
    */
  override def transform: PartialFunction[(Exp, FunctionContext), Stmt] = {
    case (callee: FuncApp, context: FunctionContext) =>
      val func = context.func

      val stmts = collection.mutable.ArrayBuffer[Stmt]()

      // check the arguments
      val termChecksOfArgs: Seq[Stmt] = callee.getArgs map (a => transform(a, context))
      stmts.appendAll(termChecksOfArgs)

      val calledFunc = functions(callee.funcname)
      val calleeArgs = callee.getArgs.map(transformExp(_, context))

      if (compareHeights(func, calledFunc)) {
        // In the same cycle. => compare

        // map of parameters in the called function to parameters in the current functions (for substitution)
        val mapFormalArgsToCalledArgs = ListMap(calledFunc.formalArgs.map(_.localVar).zip(calleeArgs): _*)

        val decOrigin = getDecreasesExp(func)
        val decDest = getDecreasesExp(calledFunc)

        assert(decOrigin.isInstanceOf[DecreasesTuple], "Checking a function with DecreasesStar for termination!" +
          "This should not happen!")

        val errTrafo = ErrTrafo({
          case AssertFailed(_, r, c) => TerminationFailed(callee, r, c)
          case d => d
        })

        val reasonTrafoFactory = SimpleReasonTrafoFactory(decOrigin.asInstanceOf[DecreasesTuple])

        val terminationCheck = createTerminationCheck(decOrigin, decDest, mapFormalArgsToCalledArgs, errTrafo, reasonTrafoFactory, context)

        val assertion = terminationCheck

        stmts.appendAll(Seq(assertion))
      } else {
        // not in the same cycle
      }
      Seqn(stmts, Nil)()
    case default => super.transform(default)
  }

  case class SimpleReasonTrafoFactory(offendingNode: DecreasesTuple) extends ReasonTrafoFactory {
    override def createNotDecrease(biggerDec: Seq[Exp], smallerDec: Seq[Exp], context: Context): ReTrafo = {
      ReTrafo({ case AssertionFalse(_) => TerminationNoDecrease(offendingNode, biggerDec, smallerDec) })
    }

    override def createNotBounded(biggerDec: Seq[Exp], context: Context): ReTrafo = {
      ReTrafo({ case AssertionFalse(_) => TerminationNoBound(offendingNode, biggerDec) })
    }

    override def createStar(context: Context): ReTrafo = {
      ReTrafo({ case AssertionFalse(_) => TerminationStar(offendingNode) })
    }
  }
}

trait FunctionContext extends Context with ProofMethodContext {
  val func: Function
}

case class TerminationNoDecrease(offendingNode: DecreasesTuple, decOrigin: Seq[Exp], decDest: Seq[Exp]) extends AbstractErrorReason {
  val id = "termination.no.decrease"
  override def readableMessage: String = s"Termination measure might not decrease. " +
    s"Assertion (${decDest.mkString(", ")})≺(${decOrigin.mkString(", ")}) might not hold."

  def withNode(offendingNode: errors.ErrorNode = this.offendingNode) = TerminationNoDecrease(this.offendingNode, decOrigin, decDest)
}

case class TerminationNoBound(offendingNode: DecreasesTuple, decExp: Seq[Exp]) extends AbstractErrorReason {
  val id = "termination.no.bound"
  override def readableMessage: String = s"Termination measure might not be bounded. " +
    s"Assertion 0≺(${decExp.mkString(", ")}) might not hold."

  def withNode(offendingNode: errors.ErrorNode = this.offendingNode) = TerminationNoBound(this.offendingNode, decExp)
}

case class TerminationStar(offendingNode: DecreasesTuple) extends AbstractErrorReason {
  val id = "termination.star"
  override def readableMessage = s"Cannot prove termination, if a function with decreasesStar is called."

  def withNode(offendingNode: errors.ErrorNode = this.offendingNode) = TerminationStar(this.offendingNode)
}