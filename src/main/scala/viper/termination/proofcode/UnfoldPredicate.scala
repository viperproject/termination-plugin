package viper.termination.proofcode

import viper.silver.ast.utility.Statements.EmptyStmt
import viper.silver.ast.{AccessPredicate, BinExp, CondExp, CurrentPerm, Domain, DomainFunc, DomainFuncApp, DomainType, Exp, FieldAccessPredicate, Fold, FuncApp, Function, If, Implies, Inhale, Int, LocalVar, LocalVarAssign, LocalVarDecl, MagicWand, NoInfo, NodeTrafo, Perm, PermExp, PermMul, Position, Predicate, PredicateAccess, PredicateAccessPredicate, Seqn, SimpleInfo, Stmt, Type, TypeVar, UnExp, Unfold, Unfolding}
import viper.silver.verifier.ConsistencyError

import scala.collection.immutable.ListMap

/**
  * Adds nested statements for the used predicates to the check code
  * and therefore also creates/manages variables representing the predicates.
  * The following features are needed in the program:
  * "nested" domain function
  * "Loc" domain
  */
trait UnfoldPredicate[C <: Context] extends ProofProgram with RewriteFunctionBody[C] {

  val nestedFunc: Option[DomainFunc] =  program.findDomainFunctionOptionally("nested")
  val locationDomain: Option[Domain] =  program.domains.find(_.name == "Loc") // findDomainOptionally()?

  private val createdLocFunctions: collection.mutable.ListMap[String, Function] = collection.mutable.ListMap[String, Function]()


  /**
    * Transforms an expression (e.g. function body) into a statement.
    * Parts of the expressions which stay expressions (e.g. the condition in a if clause)
    * are added in front as statements.
    * by the transfromExp.
    *
    * @return a statement representing the expression
    */
  override def transform: PartialFunction[(Exp, C), Stmt] = {
    case (Unfolding(pap, unfBody), c) =>
      // unfolding with nested inhale
      val permCheck = transform(pap.perm, c)

      val unfold = createUnfold(pap)

      val unfoldBody = transform(unfBody, c)
      val fold = Fold(pap)()

      Seqn(Seq(unfold, unfoldBody, fold), Nil)()

    case d => super.transform(d)
  }

  /**
    * Creates an Unfold with the given predicate access predicate and the nested relations.
    *
    * @param pap
    * @return
    */
  private def createUnfold(pap: PredicateAccessPredicate): Stmt = {

    if (locationDomain.isDefined && nestedFunc.isDefined) {
      // assign variable to "predicate" before unfold
      val varP = uniquePredLocVar(pap.loc)
      val assignP = generatePredicateAssign(pap.loc, pap.perm, varP.localVar)

      val unfold = Unfold(pap)()

      val nested: Stmt = {
        val pred = program.findPredicate(pap.loc.predicateName)
        pred.body match {
          case Some(body) =>
            if (locationDomain.isDefined && nestedFunc.isDefined) {
              val formalArgs = ListMap(pred.formalArgs.map(_.localVar).zip(pap.loc.args): _*)
              //Generate nested-assumption
              transformPredicateBody(body.replace(formalArgs), varP, pap.perm)
            } else {
              // at least one of Loc domain or nested function is not defined
              if (locationDomain.isEmpty) {
                reportLocNotDefined(pap.pos)
              }
              if (nestedFunc.isEmpty) {
                reportNestedNotDefined(pap.pos)
              }
              EmptyStmt
            }
          case None => EmptyStmt //Predicate has no body
        }
      }
      Seqn(Seq(assignP, unfold, nested), Seq(varP))()

    } else {
      // at least one of Loc domain or nested function is not defined
      if (locationDomain.isEmpty) {
        reportLocNotDefined(pap.pos)
      }
      if (nestedFunc.isEmpty) {
        reportNestedNotDefined(pap.pos)
      }
      EmptyStmt
    }


  }

  /**
    * Traverses a predicate body (once) and adds corresponding inhales of the 'nested'-Relation
    * iff a predicate is inside of this body.
    * locationDomain and nestedFun must be defined!
    *
    * @param body     the part of the predicate-body which should be analyzed
    * @param unfoldedPredVar the body of the original predicate which should be analyzed
    * @return statements with the generated inhales: (Inhale(nested(pred1, pred2)))
    */
  private def transformPredicateBody(body: Exp, unfoldedPredVar: LocalVarDecl, unfoldPermission: Exp): Stmt = {
    body match {
      case ap: AccessPredicate => ap match {
        case FieldAccessPredicate(_, _) => EmptyStmt
        case calledPred: PredicateAccessPredicate =>
          assert(locationDomain.isDefined)
          assert(nestedFunc.isDefined)

          //local variables
          val varOfCallerPred: LocalVarDecl = unfoldedPredVar
          val varOfCalleePred: LocalVarDecl = uniquePredLocVar(calledPred.loc)

          //assignment
          // perm = calledPred.perm * calleePred.perm
          val perm = PermMul(unfoldPermission, calledPred.perm)()
          val assign = generatePredicateAssign(calledPred.loc, perm, varOfCalleePred.localVar)

          //inhale nested-relation
          val params: Seq[TypeVar] = program.findDomain(nestedFunc.get.domainName).typVars
          val types: Seq[Type] =
            Seq(DomainType(locationDomain.get, ListMap()), DomainType(locationDomain.get, ListMap()), Int)

          val mapNested: ListMap[TypeVar, Type] = ListMap(params.zip(types):_*)
          val inhale = Inhale(DomainFuncApp(nestedFunc.get,
            Seq(varOfCalleePred.localVar, varOfCallerPred.localVar),
            mapNested)(calledPred.pos))(calledPred.pos)
          Seqn(Seq(assign, inhale), Seq(varOfCalleePred))(calledPred.pos)
        case mw: MagicWand =>
          sys.error(s"Unexpectedly found resource access node $mw")
      }
      case c: CondExp =>
        val thn = transformPredicateBody(c.thn, unfoldedPredVar, unfoldPermission)
        val els = transformPredicateBody(c.els, unfoldedPredVar, unfoldPermission)
        If(c.cond, Seqn(Seq(thn), Nil)(c.pos), Seqn(Seq(els), Nil)(c.pos))(c.pos)
      case i: Implies =>
        val thn = transformPredicateBody(i.right, unfoldedPredVar, unfoldPermission)
        If(i.left, Seqn(Seq(thn), Nil)(i.pos), EmptyStmt)(i.pos)
      case b: BinExp =>
        val left = transformPredicateBody(b.left, unfoldedPredVar, unfoldPermission)
        val right = transformPredicateBody(b.right, unfoldedPredVar, unfoldPermission)
        Seqn(Seq(left, right), Nil)(b.pos)
      case u: UnExp => transformPredicateBody(u.exp, unfoldedPredVar, unfoldPermission)
      case _ => EmptyStmt
    }
  }

  /**
    * Generates for a predicate and a variable the corresponding assignment
    * it generates the viper-representation of a predicate (via loc-domain and the proper domain-function)
    * and assign it to the given value
    *
    * @param pred        the predicate which defines the predicate-Domain and predicate-domainFunc
    * @param assLocation the variable, which should be assigned
    * @param permission  an optional permission value. (if None is given the permission from pred is taken)
    * @return an assignment of the given variable to the representation of a predicate with the corresponding arguments
    */
  def generatePredicateAssign(pred: PredicateAccess, permission: Exp, assLocation: LocalVar)
  : LocalVarAssign = {
    val locFunc = getLocFunction(pred.loc(program))
    val assValue = FuncApp(locFunc, pred.args :+ permission)()
    LocalVarAssign(assLocation, assValue)(pred.pos)
  }

  val initPredLocVar: scala.collection.mutable.Map[String, scala.collection.mutable.Map[PredicateAccessPredicate, LocalVarDecl]] = scala.collection.mutable.Map()

  def getInitPredLocVar(method: String, p: PredicateAccessPredicate): LocalVarDecl =
    initPredLocVar.getOrElseUpdate(method, scala.collection.mutable.Map()).getOrElseUpdate(p, uniquePredLocVar(p.loc))

  /**
    * Generator of the predicate-variables, which represents the type 'predicate'.
    * The new variable is added to neededLocalVars, which then should be added to the method.
    * locationDomain must be defined!
    * @param p predicate which defines the type of the variable
    * @return a local variable with the correct type
    */
  def uniquePredLocVar(p: PredicateAccess): LocalVarDecl = {
    assert(locationDomain.isDefined)
    val predName = p.predicateName + "_" + p.args.hashCode().toString.replaceAll("-", "_")
    val predVarName = uniqueLocalVar(predName)
    val info = SimpleInfo(Seq(p.predicateName + "_" + p.args.mkString(",")))
    val newLocalVar =
      LocalVarDecl(predVarName, DomainType(locationDomain.get,
        ListMap()))(info = info)
    newLocalVar
  }

  /**
    * Creates a function to create the representation of the predicate
    * @param pap predicate
    * @return function
    */
  private def getLocFunction(pap: Predicate): Function = {
    assert(locationDomain.isDefined)

    if (createdLocFunctions.contains(pap.name)) {
      createdLocFunctions(pap.name)
    } else {
      val uniquePredFuncName =
        uniqueName("loc_" + pap.name + "_" + pap.formalArgs.map(_.typ).mkString("_").replaceAll("\\[", "").replaceAll("\\]", ""))
      val pred = program.findPredicate(pap.name)
      val permVar =
        LocalVarDecl(uniqueNameInSet("p", pred.formalArgs.map(_.name).toSet), Perm)(pap.pos, NoInfo, NodeTrafo(pap))
      val newLocFunc =
        Function(uniquePredFuncName,
          pred.formalArgs :+ permVar,
          DomainType(locationDomain.get, ListMap()),
          Seq(PredicateAccessPredicate(PredicateAccess(pred.formalArgs.map(_.localVar), pred.name)(), permVar.localVar)(pap.pos, pap.info, pap.errT)), // or permition wildcard
          Seq(),
          None
        )(locationDomain.get.pos, locationDomain.get.info)

      createdLocFunctions(pap.name) = newLocFunc
      functions(uniquePredFuncName) = newLocFunc
      newLocFunc
    }
  }

  private val usedPredVariables: collection.mutable.Set[String] = collection.mutable.Set[String]()

  private def uniqueLocalVar(name: String): String = {
    var i = 0
    var newName = name
    while(usedPredVariables.contains(newName)){
      newName = name + i
      i += 1
    }
    usedPredVariables.add(newName)
    newName
  }


  private def uniqueNameInSet(name: String, used: Set[String]): String = {
    var i = 0
    var newName = name
    while(used.contains(newName)){
      newName = name + i
      i += 1
    }
    newName
  }

  def reportNestedNotDefined(pos: Position): Unit = {
    reportError(ConsistencyError("Nested function is needed but not defined.", pos))
  }

  def reportLocNotDefined(pos: Position): Unit = {
    reportError(ConsistencyError("Loc domain is needed but not defined.", pos))
  }
}

trait ProofMethodContext extends Context {
  val methodName: String
}