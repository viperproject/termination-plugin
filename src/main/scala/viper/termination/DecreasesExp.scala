package viper.termination

import viper.silver.ast._
import viper.silver.ast.pretty.FastPrettyPrinter.{ContOps, char, parens, space, ssep, text, toParenDoc}
import viper.silver.ast.pretty.PrettyPrintPrimitives

/**
  * Expression used to define the (possible) decreases clause (termination measure)
  */
sealed trait DecreasesExp extends ExtensionExp with Node

/**
  * Expression representing the decreases clause (termination measure).
  * @param extensionSubnodes Seq of expressions defining the termination measure (lex order)
  * @param pos Position of the node
  * @param errT Error transformer
  */
case class DecreasesTuple(extensionSubnodes: Seq[Exp] = Nil, pos: Position = NoPosition, errT: ErrorTrafo = NoTrafos) extends DecreasesExp {

  override def extensionIsPure = true

  override def typ: Type = Bool

  override def info: Info = NoInfo

  /** Pretty printing functionality as defined for other nodes in class FastPrettyPrinter.
    * Sample implementation would be text("old") <> parens(show(e)) for pretty-printing an old-expression. */
  override def prettyPrint: PrettyPrintPrimitives#Cont = text("decreases") <> parens(ssep(extensionSubnodes map (toParenDoc(_)), char(',') <> space))
}

/**
  * Expression representing the decreases star option (possibly non terminating).
  * No termination checks are done.
  * @param pos Position of the node.
  * @param errT Error transformation.
  */
case class DecreasesStar(pos: Position = NoPosition, errT: ErrorTrafo = NoTrafos) extends DecreasesExp{
  override def extensionIsPure: Boolean = true

  override def extensionSubnodes: Seq[Node] = Nil

  override def typ: Type = Bool

  /** Pretty printing functionality as defined for other nodes in class FastPrettyPrinter.
    * Sample implementation would be text("old") <> parens(show(e)) for pretty-printing an old-expression. */
  override def prettyPrint: PrettyPrintPrimitives#Cont = text("decreasesStar")

  override def info: Info = NoInfo
}
