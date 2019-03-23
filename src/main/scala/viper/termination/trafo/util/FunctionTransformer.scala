package viper.termination.trafo.util

import viper.silver.ast._
import viper.silver.ast.utility.Statements.EmptyStmt

/**
  * A basic interface which helps to write a function body (Exp) into a method body (Stmt).
  * Some basic transformations are already implemented.
  */
trait FunctionTransformer extends LocManager {

  /**
    * Transforms an expression (e.g. function body) into a statement.
    * Parts of the expressions which stay expressions (e.g. the condition in a if clause)
    * are added in front as statements.
    * Expressions which cannot be transformed to statements (e.g. literals) are replaced
    * by the transformExp.
    *
    * @return a statement representing the expression
    */
  def transformFuncBody: PartialFunction[(Exp, Context), Stmt] = {
    case (CondExp(cond, thn, els), c) =>
      val condStmt = transformFuncBody(cond, c)
      val thnStmt = transformFuncBody(thn, c)
      val elsStmt = transformFuncBody(els, c)
      val ifStmt = If(transformExp(cond, c), Seqn(Seq(thnStmt), Nil)(), Seqn(Seq(elsStmt), Nil)())()
      Seqn(Seq(condStmt, ifStmt), Nil)()
    case (Unfolding(acc, unfBody), c) =>
      val permCheck = transformFuncBody(acc.perm, c)
      val unfold = transformUnfold(acc)
      val unfoldBody = transformFuncBody(unfBody, c)
      val fold = Fold(acc)()
      Seqn(Seq(permCheck, unfold, unfoldBody, fold), Nil)()
    case (b: BinExp, c) =>
      val left = transformFuncBody(b.left, c)
      val right = transformFuncBody(b.right, c)
      // Short circuit evaluation
      b match {
        case _: Or =>
          Seqn(Seq(left,
            If(Not(b.left)(b.pos), Seqn(Seq(right), Nil)(b.pos), EmptyStmt)(b.pos)),
            Nil)(b.pos)
        case _: And =>
          Seqn(Seq(left,
            If(b.left, Seqn(Seq(right), Nil)(b.pos), EmptyStmt)(b.pos)),
            Nil)(b.pos)
        case _: Implies =>
          Seqn(Seq(left,
            If(b.left, Seqn(Seq(right), Nil)(b.pos), EmptyStmt)(b.pos)),
            Nil)(b.pos)
        case _ =>
          Seqn(Seq(left, right), Nil)(b.pos)
      }
    case (sq: SeqExp, c) => sq match {
      case ExplicitSeq(elems) =>
        Seqn(elems.map(transformFuncBody(_, c)), Nil)(sq.pos)
      case RangeSeq(low, high) =>
        Seqn(Seq(transformFuncBody(low, c),
          transformFuncBody(high, c)), Nil)(sq.pos)
      case SeqAppend(left, right) =>
        Seqn(Seq(transformFuncBody(left, c),
          transformFuncBody(right, c)), Nil)(sq.pos)
      case SeqIndex(s, idx) =>
        Seqn(Seq(transformFuncBody(s, c),
          transformFuncBody(idx, c)), Nil)(sq.pos)
      case SeqTake(s, n) =>
        Seqn(Seq(transformFuncBody(s, c),
          transformFuncBody(n, c)), Nil)(sq.pos)
      case SeqDrop(s, n) =>
        Seqn(Seq(transformFuncBody(s, c),
          transformFuncBody(n, c)), Nil)(sq.pos)
      case SeqContains(elem, s) =>
        Seqn(Seq(transformFuncBody(elem, c),
          transformFuncBody(s, c)), Nil)(sq.pos)
      case SeqUpdate(s, idx, elem) =>
        Seqn(Seq(transformFuncBody(s, c),
          transformFuncBody(idx, c),
          transformFuncBody(elem, c)), Nil)(sq.pos)
      case SeqLength(s) =>
        Seqn(Seq(transformFuncBody(s, c)), Nil)(sq.pos)
      case _: Exp => EmptyStmt
    }
    case (st: ExplicitSet, c) =>
      Seqn(st.elems.map(transformFuncBody(_, c)), Nil)(st.pos)
    case (mst: ExplicitMultiset, c) =>
      Seqn(mst.elems.map(transformFuncBody(_, c)), Nil)(mst.pos)
    case (u: UnExp, c) => transformFuncBody(u.exp, c)
    case _ => EmptyStmt
  }

  /**
    * Transforms the expressions which stay expressions.
    * @param exp to be transformed
    * @param context in which the transformation happens
    * @return the transformed expression
    */
  def transformExp: PartialFunction[(Exp, Context), Exp] = {
    case (e, c) => e
  }
}

trait Context