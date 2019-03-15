package viper.termination.proofcode.util

import viper.silver.ast._
import viper.silver.ast.utility.Rewriter.{ContextCustom, Strategy, Traverse}
import viper.silver.ast.utility.ViperStrategy
import viper.silver.verifier.errors.AssertFailed
import viper.termination.proofcode.Methods
import viper.termination.{DecreasesExp, DecreasesTuple}

import scala.collection.immutable.ListMap

trait MethodCheck extends ProgramManager with DecreasesCheck with LocManager{

  def cluster(m1: String, m2: String): Boolean = {
    val method1 = methods.get(m1)
    val method2 = methods.get(m2)
    if (method1.isDefined && method2.isDefined){
      Methods.getMethodCluster(method1.get, program).contains(method2.get)
    }else{
      false
    }
  }

  val methodsDec: Map[String, DecreasesExp]
  def getMethodDecreasesExp(method: String): DecreasesExp = methodsDec.getOrElse(method, {
    val m = methods.get(method)
    if(m.isDefined) {
      DecreasesTuple(m.get.formalArgs.map(_.localVar), m.get.pos, NodeTrafo(m.get))
    }else{
      DecreasesTuple()
    }
  })

  def methodStrategy(context: ProofMethodContext): Strategy[Node, ContextCustom[Node, ProofMethodContext]] =
    ViperStrategy.CustomContext[ProofMethodContext](methodTransformer, context, t = Traverse.BottomUp)

  private def methodTransformer: PartialFunction[(Node, ProofMethodContext), Node] = {
    case (mc: MethodCall, context: ProofMethodContext) if cluster(mc.methodName, context.methodName) =>  // have to perform termination check
      // map of parameters
      val calledMethod = methods(mc.methodName)
      val mapFormalArgsToCalledArgs = ListMap(calledMethod.formalArgs.map(_.localVar).zip(mc.args): _*)
      val decOrigin = getMethodDecreasesExp(context.methodName)
      val decDest = getMethodDecreasesExp(mc.methodName)

      assert(decOrigin.isInstanceOf[DecreasesTuple], "Checking a method with DecreasesStar for termination!" +
        "This should not happen!")

      val errTrafo = ErrTrafo({
        case AssertFailed(_, r, c) => TerminationFailed(mc, r, c)
        case d => d
      })

      val reasonTrafoFactory = SimpleReasonTrafoFactory(decOrigin.asInstanceOf[DecreasesTuple])

      val terminationCheck = createTerminationCheck(decOrigin, decDest, mapFormalArgsToCalledArgs, errTrafo, reasonTrafoFactory, context)

      val assertion = terminationCheck

      Seqn(Seq(assertion, mc), Nil)(mc.pos, NoInfo, NodeTrafo(mc))
    case (u: Unfold, c: ProofMethodContext) => transformUnfold(u.acc)
  }
}
