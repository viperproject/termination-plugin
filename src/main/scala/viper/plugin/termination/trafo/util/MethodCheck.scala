package viper.plugin.termination.trafo.util

import viper.silver.ast._
import viper.silver.ast.utility.Rewriter.{ContextCustom, Strategy, Traverse}
import viper.silver.ast.utility.ViperStrategy
import viper.silver.verifier.errors.AssertFailed
import viper.plugin.termination.{DecreasesExp, DecreasesStar, DecreasesTuple}

import scala.collection.immutable.ListMap

/**
  * Creates termination checks for methods.
  */
trait MethodCheck extends CheckProgramManager with DecreasesCheck with PredicateInstanceManager{

  /**
    * Checks if two methods call each other recursively (also indirect) (same cluster)
    * @param m1 method one
    * @param m2 method two
    * @return true iff in same cluster
    */
  def sameCluster(m1: String, m2: String): Boolean = {
    val method1 = methods.get(m1)
    val method2 = methods.get(m2)
    if (method1.isDefined && method2.isDefined){
      Methods.getMethodCluster(method1.get, program).contains(method2.get)
    }else{
      false
    }
  }

  /**
    * DecreasesExp for methods defined by the user.
    */
  val methodsDec: Map[String, DecreasesExp]

  /**
    * @param method name
    * @return DecreasesExp defined by the user if exists, otherwise a DecreasesTuple containing the methods parameter.
    */
  def getMethodDecreasesExp(method: String): DecreasesExp = methodsDec.getOrElse(method, {
    val m = methods.get(method)
    if(m.isDefined) {
      DecreasesTuple(m.get.formalArgs.map(_.localVar))(m.get.pos, NoInfo, NodeTrafo(m.get))
    }else{
      DecreasesTuple()()
    }
  })

  /**
    * Creates a new program with the additional features.
    * Should only be called once.
    *
    * @return new program.
    */
  override protected def generateCheckProgram(): Program = {
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
    super.generateCheckProgram()
  }

  /**
    * @param context to the body (method name etc.)
    * @return Strategy to be used to transform a methods body.
    */
  def methodStrategy(context: ProofMethodContext): Strategy[Node, ContextCustom[Node, ProofMethodContext]] =
    ViperStrategy.CustomContext[ProofMethodContext](methodTransformer, context, t = Traverse.BottomUp)

  private def methodTransformer: PartialFunction[(Node, ProofMethodContext), Node] = {
    case (mc: MethodCall, context: ProofMethodContext) if sameCluster(mc.methodName, context.methodName) =>  // have to perform termination check
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

  case class MContext(override val methodName: String) extends ProofMethodContext
}
