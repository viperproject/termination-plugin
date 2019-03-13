package viper.termination.proofcode

import viper.silver.ast.utility.Statements.EmptyStmt
import viper.silver.ast.{AccessPredicate, BinExp, CondExp, Domain, DomainFunc, DomainFuncApp, DomainType, Exp, FieldAccessPredicate, Fold, Function, If, Implies, Inhale, Int, LocalVar, LocalVarAssign, LocalVarDecl, MagicWand, Position, PredicateAccess, PredicateAccessPredicate, Program, Seqn, SimpleInfo, Stmt, Type, TypeVar, UnExp, Unfold, Unfolding}
import viper.silver.verifier.ConsistencyError

import scala.collection.immutable.ListMap

/**
  * Adds nested statements for the used predicates to the check code
  * and therefore also creates/manages variables representing the predicates.
  * The following features are needed in the program:
  * "nested" domain function
  * "Loc" domain
  */
trait UnfoldPredicate[C <: ProofMethodContext] extends ProofProgram with RewriteFunctionBody[C] {

  val nestedFunc: Option[DomainFunc] =  program.findDomainFunctionOptionally("nested")
  val locationDomain: Option[Domain] =  program.domains.find(_.name == "Loc") // findDomainOptionally()?

  // local variables for methods. Have to be added to the created method
  val neededLocalVars: collection.mutable.ListMap[String, collection.mutable.ListMap[String, LocalVarDecl]] = collection.mutable.ListMap[String, collection.mutable.ListMap[String, LocalVarDecl]]()

  private val neededLocFunctions: collection.mutable.ListMap[String, DomainFunc] = collection.mutable.ListMap[String, DomainFunc]()

  /**
    * Creates a new program with the needed fields added to it
    * @return a program
    */
  override protected def createCheckProgram(): Program = {

    if(neededLocFunctions.nonEmpty){
      assert(locationDomain.isDefined)
      val newLocDom = Domain(locationDomain.get.name,
        neededLocFunctions.values.toSeq,
        locationDomain.get.axioms,
        locationDomain.get.typVars)(locationDomain.get.pos, locationDomain.get.info, locationDomain.get.errT)

      domains(newLocDom.name) = newLocDom
    }

    super.createCheckProgram()
  }


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
      val unfold = Unfold(pap)()

      val nested: Stmt = {
        val pred = program.findPredicate(pap.loc.predicateName)
        pred.body match {
          case Some(body) =>
            if (locationDomain.isDefined && nestedFunc.isDefined) {
              val formalArgs = pred.formalArgs map (_.localVar)
              //Generate nested-assumption
              transformPredicateBody(body.replace(ListMap(formalArgs.zip(pap.loc.args): _*)), pap, c)
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
          //Predicate has no body
          case None => EmptyStmt
        }
      }

      val unfoldBody = transform(unfBody, c)
      val fold = Fold(pap)()
      Seqn(Seq(unfold, nested, unfoldBody, fold), Nil)()

    case d => super.transform(d)
  }

  /**
    * Traverses a predicate body (once) and adds corresponding inhales of the 'nested'-Relation
    * iff a predicate is inside of this body.
    * locationDomain and nestedFun must be defined!
    *
    * @param body     the part of the predicate-body which should be analyzed
    * @param origPred the body of the original predicate which should be analyzed
    * @return statements with the generated inhales: (Inhale(nested(pred1, pred2)))
    */
  private def transformPredicateBody(body: Exp, origPred: PredicateAccessPredicate, context: ProofMethodContext): Stmt = {
    body match {
      case ap: AccessPredicate => ap match {
        case FieldAccessPredicate(_, _) => EmptyStmt
        case calledPred: PredicateAccessPredicate =>
          assert(locationDomain.isDefined)
          assert(nestedFunc.isDefined)

          //local variables
          val varOfCallerPred: LocalVar = uniquePredLocVar(origPred.loc, context)
          val varOfCalleePred: LocalVar = uniquePredLocVar(calledPred.loc, context)

          //assign
          val assign1 = generateAssign(origPred, varOfCallerPred)
          val assign2 = generateAssign(calledPred, varOfCalleePred)

          //inhale nested-relation
          val params: Seq[TypeVar] = program.findDomain(nestedFunc.get.domainName).typVars
          val types: Seq[Type] =
            Seq(DomainType(locationDomain.get, ListMap()), DomainType(locationDomain.get, ListMap()), Int)

          val mapNested: ListMap[TypeVar, Type] = ListMap(params.zip(types):_*)
          val inhale = Inhale(DomainFuncApp(nestedFunc.get,
            Seq(varOfCalleePred, varOfCallerPred),
            mapNested)(calledPred.pos))(calledPred.pos)
          Seqn(Seq(assign1, assign2, inhale), Nil)(calledPred.pos)
        case mw: MagicWand =>
          sys.error(s"Unexpectedly found resource access node $mw")
      }
      case c: CondExp =>
        val thn = transformPredicateBody(c.thn, origPred, context)
        val els = transformPredicateBody(c.els, origPred, context)
        If(c.cond, Seqn(Seq(thn), Nil)(c.pos), Seqn(Seq(els), Nil)(c.pos))(c.pos)
      case i: Implies =>
        val thn = transformPredicateBody(i.right, origPred, context)
        If(i.left, Seqn(Seq(thn), Nil)(i.pos), EmptyStmt)(i.pos)
      case b: BinExp =>
        val left = transformPredicateBody(b.left, origPred, context)
        val right = transformPredicateBody(b.right, origPred, context)
        Seqn(Seq(left, right), Nil)(b.pos)
      case u: UnExp => transformPredicateBody(u.exp, origPred, context)
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
    * @param argMap      an optional mapping used for replacing the arguments of the predicate
    * @return an assignment of the given variable to the representation of a predicate with the corresponding arguments
    */
  def generateAssign(pred: PredicateAccessPredicate, assLocation: LocalVar, argMap: ListMap[Exp, Exp] = ListMap.empty)
  : LocalVarAssign = {
    val domainFunc = addPredicateDomainFunction(pred)
    val typVarMap: ListMap[TypeVar, Type] = ListMap()
    val assValue = DomainFuncApp(domainFunc, pred.loc.args.map(_.replace(argMap)), typVarMap)(pred.pos)
    LocalVarAssign(assLocation, assValue)(pred.pos)
  }

  /**
    * Generator of the predicate-variables, which represents the type 'predicate'.
    * The new variable is added to neededLocalVars, which then should be added to the method.
    * locationDomain must be defined!
    * @param p predicate which defines the type of the variable
    * @return a local variable with the correct type
    */
  def uniquePredLocVar(p: PredicateAccess, context: ProofMethodContext): LocalVar = {
    assert(locationDomain.isDefined)

    val proofMethod = context.methodName
    val predVarName = p.predicateName + "_" + p.args.hashCode().toString.replaceAll("-", "_")
    if (!neededLocalVars.contains(proofMethod)){
      neededLocalVars(proofMethod) = collection.mutable.ListMap()
    }
    if (neededLocalVars(proofMethod).contains(predVarName)) {
      //Variable already exists
      neededLocalVars(proofMethod)(predVarName).localVar
    } else {
      val info = SimpleInfo(Seq(p.predicateName + "_" + p.args.mkString(",")))
      val newLocalVar =
        LocalVar(predVarName)(DomainType(locationDomain.get,
          ListMap()),
          info = info)
      neededLocalVars(proofMethod)(predVarName) = LocalVarDecl(newLocalVar.name, newLocalVar.typ)(newLocalVar.pos, info)
      newLocalVar
    }
  }

  /**
    * Creates a domain function to create the representation of the predicate
    * @param pap predicate
    * @return domain function
    */
  private def addPredicateDomainFunction(pap: PredicateAccessPredicate): DomainFunc = {
      if (neededLocFunctions.contains(pap.loc.predicateName)) {
        neededLocFunctions(pap.loc.predicateName)
      } else {
        val uniquePredFuncName =
          uniqueName("loc_" + pap.loc.args.map(_.typ).mkString("_").replaceAll("\\[", "").replaceAll("\\]", ""))
        val pred = program.findPredicate(pap.loc.predicateName)
        val newLocFunc =
          DomainFunc(uniquePredFuncName,
            pred.formalArgs,
            DomainType(locationDomain.get,
              ListMap())
          )(locationDomain.get.pos, locationDomain.get.info, locationDomain.get.name, locationDomain.get.errT)

        neededLocFunctions(pap.loc.predicateName) = newLocFunc
        newLocFunc
      }
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