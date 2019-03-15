package viper.termination.proofcode.util

import viper.silver.ast._
import viper.silver.verifier.reasons.AssertionFalse
import viper.silver.verifier.{AbstractErrorReason, errors}
import viper.termination.{DecreasesExp, DecreasesTuple}

//### SIMPLE REASONS ###
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


//### PATH REASONS ###
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
