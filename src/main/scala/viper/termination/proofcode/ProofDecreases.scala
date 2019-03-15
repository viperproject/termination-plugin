package viper.termination.proofcode

import viper.silver.ast.utility.Statements.EmptyStmt
import viper.silver.ast.{And, Assert, DomainFunc, DomainFuncApp, EqCmp, ErrTrafo, Exp, FalseLit, FuncApp, LocalVar, LocalVarDecl, Node, Or, Position, PredicateAccessPredicate, ReTrafo, Seqn, Stmt}
import viper.silver.verifier.{AbstractVerificationError, ConsistencyError, ErrorReason, errors}

import scala.collection.immutable.ListMap

/**
  * A basic interface to help create termination checks.
  * Therefore it needs following things in the program:
  * "decreasing" domain function
  * "bounded" domain function
  *
  * It adds dummy function to the program if needed.
  */
trait ProofDecreases[C <: ProofMethodContext] extends ProofProgram with UnfoldPredicate[C] {

  val decreasingFunc: Option[DomainFunc] = program.findDomainFunctionOptionally("decreasing")
  val boundedFunc: Option[DomainFunc] =  program.findDomainFunctionOptionally("bounded")

  /**
    * Creates a termination check.
    * If decreasing and bounded functions are not defined a consistency error is reported.
    * @param biggerDec DecreaseExp of the function currently checked
    * @param smallerDec DecreaseExp of the function called
    * @param argMap Substitutions for smallerDec
    * @param errTrafo for termination related assertions
    * @param reasonTrafoFactory for termination related assertion reasons
    * @param context of the current termination check
    * @return termination check as a Assert Stmt (if decreasing and bounded are defined, otherwise EmptyStmt)
    */
  def createTerminationCheck(biggerDec: DecreasesExp, smallerDec: DecreasesExp, argMap: Map[LocalVar, Node],
                             errTrafo: ErrTrafo, reasonTrafoFactory: ReasonTrafoFactory, context: C): Stmt = {
    (biggerDec, smallerDec) match {
      case (DecreasesTuple(_,_,_), DecreasesStar(_,_)) =>
        val reTStar = reasonTrafoFactory.createStar(context)
        Assert(FalseLit()(errT = reTStar))(errT = errTrafo)
      case (DecreasesTuple(biggerExp,_,_), DecreasesTuple(smallerExp,_,_)) =>
        // decreasing and bounded functions are needed
        if(decreasingFunc.isDefined && boundedFunc.isDefined) {
          // trims to the longest commonly typed prefix
          val (bExp, sExp) = (biggerExp zip smallerExp.map(_.replace(argMap))).takeWhile(exps => exps._1.typ == exps._2.typ).unzip

          val reTBound = reasonTrafoFactory.createNotBounded(bExp, context)

          val reTDec = reasonTrafoFactory.createNotDecrease(bExp, sExp, context)

          val checkableBiggerExp = bExp.map({
            case pa: PredicateAccessPredicate =>
              if (locationDomain.isDefined) {
                // use the init predicate variable
                val varOfCalleePred = getInitPredLocVar(context.methodName, pa)
                varOfCalleePred.localVar
              } else {
                reportLocNotDefined(biggerDec.pos)
                pa
              }
            //case unfold: Unfolding => Old(unfold)(unfold.pos)
            case default => default
          })

          var newVarPred = scala.collection.mutable.Map[PredicateAccessPredicate, LocalVarDecl]()

          val checkableSmallerExp = sExp.map({
            case pa: PredicateAccessPredicate =>
              if (locationDomain.isDefined) {
                val varOfCalleePred = uniquePredLocVar(pa.loc)
                newVarPred(pa) = varOfCalleePred
                varOfCalleePred.localVar
              } else {
                reportLocNotDefined(biggerDec.pos)
                pa
              }
            case default => default
          })

          val newVarPredAss: Seq[Stmt] = newVarPred.map(v => generatePredicateAssign(v._1.loc, v._1.perm, v._2.localVar)).toSeq

          val check = createTerminationCheckExp(checkableBiggerExp, checkableSmallerExp, reTDec, reTBound)
          val assert = Assert(check)(errT = errTrafo)

          Seqn(newVarPredAss :+ assert, newVarPred.values.toSeq)()
        }else{
          // at least of the needed functions is not defined
          if (decreasingFunc.isEmpty){
            reportDecreasingNotDefined(biggerDec.pos)
          }
          if (boundedFunc.isEmpty){
            reportBoundedNotDefined(biggerDec.pos)
          }
          EmptyStmt
        }
      case default =>
        assert(assertion = false, "Checking a function with DecreasesStar for termination!" +
          "This should not happen!")
        Assert(FalseLit()())(errT = errTrafo)
    }
  }

  /**
    * If expressions are not empty
    * creates Expression to check decrease and bounded of lexicographical order
    * (decreasing(s,b) && bounded(b)) || (s==b && ( (decr...
    * decreasing and bounded must be defined!
    * @param biggerExp [b,..] (can also be empty)
    * @param smallerExp [s,..] same size as biggerExp
    * @return expression or false if expression is empty
    */
  private def createTerminationCheckExp(biggerExp: Seq[Exp], smallerExp: Seq[Exp], decrReTrafo: ReTrafo, boundReTrafo: ReTrafo): Exp = {
    assert(biggerExp.size == smallerExp.size)
    assert(decreasingFunc.isDefined)
    assert(boundedFunc.isDefined)

    if (biggerExp.isEmpty){
      FalseLit()(errT = decrReTrafo)
    }else {
      val paramTypesDecr = decreasingFunc.get.formalArgs map (_.typ)
      val argTypeVarsDecr = paramTypesDecr.flatMap(p => p.typeVariables)
      val paramTypesBound = boundedFunc.get.formalArgs map (_.typ)
      val argTypeVarsBound = paramTypesBound.flatMap(p => p.typeVariables)

      def createExp(biggerExp: Seq[Exp], smallerExp: Seq[Exp]): Exp = {
        assert(biggerExp.nonEmpty)
        assert(biggerExp.size == smallerExp.size)
        val bigger = biggerExp.head
        val smaller = smallerExp.head
        val dec = DomainFuncApp(decreasingFunc.get,
          Seq(smaller, bigger),
          ListMap(argTypeVarsDecr.head -> smaller.typ,
            argTypeVarsDecr.last -> bigger.typ))(errT = decrReTrafo)

        val bound = DomainFuncApp(boundedFunc.get,
          Seq(bigger),
          ListMap(argTypeVarsDecr.head -> bigger.typ,
            argTypeVarsDecr.last -> bigger.typ
          ))(errT = boundReTrafo)

        val andPart = And(dec, bound)()

        if (biggerExp.size == 1) {
          // no next elements
          andPart
        } else {
          val eq = EqCmp(smaller, bigger)(errT = decrReTrafo)
          val next = createExp(biggerExp.tail, smallerExp.tail)
          val nextPart = And(eq, next)()
          Or(andPart, nextPart)()
        }
      }

      createExp(biggerExp, smallerExp)
    }
  }

  def reportDecreasingNotDefined(pos: Position): Unit = {
    reportError(ConsistencyError("Decreasing function is needed but not defined.", pos))
  }

  def reportBoundedNotDefined(pos: Position): Unit = {
    reportError(ConsistencyError("Bounded function is needed but not defined.", pos))
  }
}

/**
  * Error for all termination related failed assertions.
  */
case class TerminationFailed(offendingNode: FuncApp, reason: ErrorReason, override val cached: Boolean = false) extends AbstractVerificationError {
  val id = "termination.failed"
  val text = s"Function might not terminate."

  def withNode(offendingNode: errors.ErrorNode = this.offendingNode) = TerminationFailed(this.offendingNode, this.reason)
  def withReason(r: ErrorReason) = TerminationFailed(offendingNode, r)
}

/**
  * Interface for factories creating trafos of non-termination reasons.
  */
trait ReasonTrafoFactory{
  def createNotDecrease(biggerDec: Seq[Exp], smallerDec: Seq[Exp], context: Context): ReTrafo
  def createNotBounded(biggerDec: Seq[Exp], context: Context): ReTrafo
  def createStar(context: Context): ReTrafo
}