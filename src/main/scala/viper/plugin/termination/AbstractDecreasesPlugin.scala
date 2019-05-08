// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2011-2019 ETH Zurich.

package viper.plugin.termination

import viper.silver.ast.{Assert, Call, Exp, Method, NodeTrafo, PredicateAccess, PredicateAccessPredicate, Program, Function}
import viper.silver.ast.utility.{Functions, ViperStrategy}
import viper.silver.parser._
import viper.silver.plugin.SilverPlugin
import viper.silver.verifier.errors.AssertFailed
import viper.silver.verifier.{ConsistencyError, Failure, Success, VerificationResult}

/**
  * Interface for plugins which use decreases clauses (DecreasesExp) of functions and methods.
  * This includes following steps (in the according stages):
  *
  * (beforeResolve, beforeMethodFilter): Transformation of decreases clauses defined in Viper
  * (e.g. in function postcondition: ensures decreases(list(xs))) into type checked DecreasesExp.
  *
  * (beforeVerify): Extract DecreasesExp from the program and call transformToCheckProgram.
  */
trait AbstractDecreasesPlugin extends SilverPlugin {

  // decreases keywords
  private val DECREASES = "decreases"
  private val DECREASESSTAR = "decreasesStar"

  /** Called after parse AST has been constructed but before identifiers are resolved and the program is type checked.
    *
    * Replaces all decreases function calls in postconditions with decreasesN function calls and
    * adds a domain with the necessary decreasesN functions.
    * (decreasesN functions are domain functions with N parameters of arbitrary type)
    *
    * Replaces all predicates acc in decreases function calls, replaces them with function calls, representing the
    * the predicate acc.
    *
    * Replaces all decreasesStar function calls in postconditions with unique decreasesStar function calls
    * and adds a domain with the unique function.
    *
    * @param input Parse AST possibly containing the decreases clauses in postconditions
    * @return Modified Parse AST which now should type check
    */
  override def beforeResolve(input: PProgram): PProgram = {

    /**
      * Perform the described replacements.
      */
    def fixPDecreasesClauses(exp: PExp): PExp = exp match {
      case call: PCall if call.opName == DECREASES =>
        // replace decreases call
        val functionName = getDecreasesNFunction(call.args.length)
        // replace predicates
        val newArgs = call.args.map {
          case predicateCall: PCall if input.predicates.exists(_.idndef.name == predicateCall.idnuse.name) =>
            // a predicate with the same name exists
            fixAccessPredicate(predicateCall)
          case pap: PAccPred if input.predicates.exists(_.idndef.name == pap.loc.idnuse.name) =>
            // a predicate with the same name exists
            fixAccessPredicate(pap.loc, pap.perm)
          case default => default
        }
        call.copy(func = PIdnUse(functionName), args = newArgs).setPos(call)
      case call: PCall if call.opName == DECREASESSTAR =>
        // replace decreasesStar call
        // number of arguments is checked by the type checker.
        call.copy(func = PIdnUse(getDecreasesStarFunction)).setPos(call)
      case default => default
    }

    /**
      * Replaces predicate access with function call to domain function representing the predicate.
      * The predicate must be in the program!
      * The arguments of the new function are the arguments of the predicate
      * plus the permission expression as last argument.
      * @param pap the predicate access call to be replaced
      * @param perm the permission of the predicate access
      * @return function call to the new function
      */
    def fixAccessPredicate(pap: POpApp, perm: PExp = PFullPerm()): PCall = {
      // a predicate with the same name must exists
      assert(input.predicates.exists(_.idndef.name.equals(pap.opName)))
      val predicate = input.predicates.find(_.idndef.name.equals(pap.opName)).get
      val formalArg = predicate.formalArgs
      // use the same arguments!
      val function = addPredicateFunctions(pap.opName, formalArg)
      PCall(PIdnUse(function), pap.args :+ perm).setPos(pap)
    }

    // check all postconditions of functions
    val functions = input.functions.map(function => {
      val posts = function.posts.map(fixPDecreasesClauses)
      function.copy(posts = posts).setPos(function)
    })

    // check all postconditions of methods
    val methods = input.methods.map(method => {
      val posts = method.posts.map(fixPDecreasesClauses)
      method.copy(posts = posts).setPos(method)
    })

    // all domains including the new helper domain
    val domains = input.domains :+
      createHelperDomain(getHelperDomain, getDecreasesStarFunction, decreasesNFunctions.toMap, predicateFunctions.toMap)

    input.copy(functions = functions, methods = methods, domains = domains).setPos(input)
  }

  /**
    * @return unique name for the decreasesStar function
    */
  private val getDecreasesStarFunction: String = {
    "$decreasesStar"
  }

  /**
    * All needed decreasesN functions with N arguments
    * (N -> decreasesNFunction name)
    * Must be invertible!
    */
  private val decreasesNFunctions = collection.mutable.Map[Int, String]()

  /**
    * Adds a new function name to the decreasesNFunctions map if none exists for this many arguments.
    * @param argsSize: number of arguments needed
    * @return name of decreaseN function
    */
  private def getDecreasesNFunction(argsSize: Int): String = {
    decreasesNFunctions.getOrElseUpdate(argsSize, s"$$decreases$argsSize")
  }

  /**
    * All needed functions representing a predicate in the decrease clause
    * ((predicateName, arguments :+ perm) -> functionName)
    * Must be invertible
    */
  private val predicateFunctions = collection.mutable.Map[(String, Seq[PFormalArgDecl]), String]()

  /**
    * Adds a new predicate representing function's name to the predicateFunction map
    * if none exists for this predicate.
    * @param predicateName identifying the predicate
    * @param params of the predicate
    * @return name of the function
    */
  private  def addPredicateFunctions(predicateName: String, params: Seq[PFormalArgDecl]): String = {
    predicateFunctions.getOrElseUpdate((predicateName, params),
      s"$$pred_${predicateName}_${params.flatten(_.idndef.name).mkString("$")}")
  }

  /**
    * @return unique name for the helper domain containing all the new functions
    */
  private val getHelperDomain: String = {
    "$HelperDomain"
  }

  /**
    * Creates a domain with parameter types and containing the necessary new domain functions.
    * @param name (unique) name of the domain
    * @param decreasesStar (unique) name of the decreasesStar function
    * @param decreasesN (unique) names of decreasesN functions (and number of arguments: N)
    * @param predicates (unique) names of functions representing predicates (and number of arguments)
    * @return domain containing all the wanted functions.
    */
  private def createHelperDomain(name: String, decreasesStar: String,
                                 decreasesN: Map[Int, String],
                                 predicates: Map[(String, Seq[PFormalArgDecl]), String]): PDomain = {
    val domainIdDef = PIdnDef(name)
    val domainIdUse = PIdnUse(name)

    // decreases star domain function
    val decreasesStarFunction = PDomainFunction(PIdnDef(decreasesStar), Nil, PPrimitiv(TypeHelper.Bool.name), unique = false)(domainIdUse)

    // number of type parameters needed
    val maxArgs: Int = decreasesN.keySet match {
      case d if d.isEmpty => 0
      case d => d.max
    }
    // all type parameters
    val typeVars = Seq.tabulate(maxArgs)(i => PTypeVarDecl(PIdnDef(s"T$i")))
    // list of arguments with the type parameter
    val formalArgs: Seq[PFormalArgDecl] = typeVars.map(t => PFormalArgDecl(PIdnDef(s"v${t.idndef.name}"), PDomainType(PIdnUse(t.idndef.name), Seq())))
    // all needed decreasesN functions
    val decreasesFunctions: Seq[PDomainFunction] = decreasesN.map {
      case (argsSize, functionName) =>
        PDomainFunction(PIdnDef(functionName), formalArgs.take(argsSize), PPrimitiv(TypeHelper.Bool.name), unique = false)(domainIdUse)
    }.toSeq

    // all needed predicate functions
    val predicateFunctions: Seq[PDomainFunction] = predicates.map {
      case ((_, args), function) =>
        // add permission as last argument
        val perm = getUniqueName("permission", args.map(_.idndef.name).toSet)
        val permArgDecl = PFormalArgDecl(PIdnDef(perm), TypeHelper.Perm)
        PDomainFunction(PIdnDef(function), args :+ permArgDecl, PPrimitiv(TypeHelper.Bool.name), unique = false)(domainIdUse)
    }.toSeq

    PDomain(domainIdDef, typeVars, decreasesFunctions ++ predicateFunctions :+ decreasesStarFunction , Seq())
  }

  /**
    * @param whish to be unique
    * @param used names
    * @return unique name similar to wish
    */
  private def getUniqueName(whish: String, used: Set[String]): String = {
    var i = 0
    var newName = whish
    while(used.contains(newName)){
      newName = whish + i
      i += 1
    }
    newName
  }


  /** Called after parse AST has been translated into the normal AST but before methods to verify are filtered.
    *
    * Replace all decreasesN function calls in postconditions (from the beforeResolve step)
    * with DecreasesTuple expressions.
    *
    * Replaces all function calls representing a predicate acc (from the beforeResolve step)
    * with predicate acc.
    *
    * Replaces all decreasesStar functions in postconditions (from the beforeResolve step)
    * with DecreasesStar expressions.
    *
    * @param input AST
    * @return Modified AST possibly with DecreasesExp (DecreasesTuple or DecreasesStar) in the postconditions of functions.
    */
  override def beforeMethodFilter(input: Program): Program = {

    val helperDomain = getHelperDomain
    val decStarFunc = getDecreasesStarFunction
    // invert maps once for convenience (and performance)
    val decNFuncInverted = decreasesNFunctions.toMap.map(_.swap)
    val predFuncInverted = predicateFunctions.toMap.map(_.swap)

    /**
      * Perform the described replacements.
      */
    def createDecreasesExp(exp: Exp): Exp = exp match {
      case c: Call if c.callee == decStarFunc =>
        // replace all decreasesStar functions with DecreasesStar
        DecreasesStar()(pos = c.pos, errT = NodeTrafo(c))
      case c: Call if decNFuncInverted.contains(c.callee) =>
        // replace all decreasesN functions with DecreasesTuple
        assert(c.args.size == decNFuncInverted(c.callee))
        val newArgs = c.args map {
          // replace all predicate functions with the PredicateAccessPredicate
          case p: Call if predFuncInverted.contains(p.callee) =>
            val mapResult = predFuncInverted(p.callee)
            assert(p.args.size - 1 == mapResult._2.size) // + permission argument
            val pa = PredicateAccess(p.args.init, mapResult._1)(p.pos, p.info, p.errT)
            PredicateAccessPredicate(pa, perm = p.args.last)(p.pos, p.info, p.errT)
          case default => default
        }
        DecreasesTuple(newArgs)(pos = c.pos, errT = NodeTrafo(c))
      case p => p
    }

    ViperStrategy.Slim({
      case p: Program =>
        // remove the helper domain
        val domains = p.domains.filterNot(d => d.name.equals(helperDomain))
        p.copy(domains = domains)(p.pos, p.info, p.errT)
      case f: Function =>
        // replace decreasesN calls in postconditions with DecreasesExp
        val posts = f.posts map createDecreasesExp
        f.copy(posts = posts)(f.pos, f.info, f.errT)
      case m: Method =>
        // replace decreasesN calls in postconditions with DecreasesExp
        val posts = m.posts map createDecreasesExp
        m.copy(posts = posts)(m.pos, m.info, m.errT)
    }).execute(input)
  }

  /** Called after methods are filtered but before the verification by the backend happens.
    *
    * Checks that functions are not (indirectly) recursively defined via its DecreasesExp
    * Check that not multiple DecreasesExp are defined for one function or method
    * (otherwise consistency errors are reported and the verification stopped).
    *
    * Extracts all DecreasesExp from the program (postconditions of functions and methods)
    * and uses them to call transformToCheckProgram.
    * @param input AST possibly with DecreasesExp (DecreasesTuple or DecreasesStar).
    * @return Modified AST without DecreasesExp.
    */
  override def beforeVerify(input: Program): Program = {
    val errors = checkNoFunctionRecursesViaDecreasesClause(input) ++ checkNoMultipleDecreasesClause(input)
    if (errors.nonEmpty){
      errors.foreach(reportError)
      return input
    }

    // extract decreases expressions from the program
    val extractedDecreasesExp = extractDecreasesExp(input)

    val newProgram: Program = extractedDecreasesExp._1
    val functionDecreasesMap = extractedDecreasesExp._2
    val methodDecreasesMap = extractedDecreasesExp._3

    transformToCheckProgram(newProgram, functionDecreasesMap, methodDecreasesMap)
  }


  /** Called after the verification. Error transformation should happen here.
    * This will only be called if verification took place.
    *
    * @param input Result of verification
    * @return Modified result
    */
  override def mapVerificationResult(input: VerificationResult): VerificationResult = {
    input match {
      case Success => input
      case Failure(errors) => Failure(errors.map({
        case a@AssertFailed(Assert(_), _, _) => a.transformedError()
        case e => e
      }))
    }
  }

  /**
    * Creates a Program containing all the wanted termination checks (defined by a concrete plugin)
    *
    * @param input verifiable program (i.e. no DecreasesExp in postconditions)
    * @param functionDecreasesMap all decreases exp of functions (defined by the user)
    * @param methodDecreasesMap all decreases exp of methods (defined by the user)
    * @return a program with the additional termination checks (including the input program)
    */
  def transformToCheckProgram(input: Program,
                              functionDecreasesMap: Map[Function, DecreasesExp] = Map.empty[Function, DecreasesExp],
                              methodDecreasesMap: Map[String, DecreasesExp] = Map.empty[String, DecreasesExp])
                              : Program

  /**
    * Checks if a function is defined recursively via its decrease clause (DecreasesExp in postconditions),
    * which is not allowed in this case.
    * @param program containing functions to be checked
    * @return consistency error if cycles are detected.
    */
  private def checkNoFunctionRecursesViaDecreasesClause(program: Program): Seq[ConsistencyError] = {

    def functionDecs(function:Function) = function.posts.filter(p => p.isInstanceOf[DecreasesExp])

    var errors = Seq.empty[ConsistencyError]
    Functions.findFunctionCyclesVia(program, functionDecs) foreach { case (func, cycleSet) =>
      var msg = s"Function ${func.name} recurses via its decreases clause.\n"

      if (cycleSet.nonEmpty) {
        msg = s"$msg: The cycle contains the function(s) ${cycleSet.map(_.name).mkString(", ")}."
      }
      errors :+= ConsistencyError(msg, func.pos)
    }
    errors
  }

  /**
    * Checks if each function only contains at max one decreases clause (DecreasesExp in postconditions).
    * @param program containing the functions to be checked
    * @return consistency error if function with multiple decreases clauses are detected.
    */
  private def checkNoMultipleDecreasesClause(program: Program): Seq[ConsistencyError] = {
    var errors = Seq.empty[ConsistencyError]
    program.functions.foreach(f => {
      if (f.posts.count(_.isInstanceOf[DecreasesExp]) > 1){
        // more than one decreases clause detected for function f
        val msg = s"Function ${f.name} contains more than one decreases clause."
        errors :+= ConsistencyError(msg, f.pos)
      }
    })
    program.methods.foreach(m => {
      if (m.posts.count(_.isInstanceOf[DecreasesExp]) > 1){
        // more than one decreases clause detected for method m
        val msg = s"Method ${m.name} contains more than one decreases clause."
        errors :+= ConsistencyError(msg, m.pos)
      }
    })
    errors
  }

  /**
    * Extracts one(!) DecreasesExp in postconditions of functions and methods.
    * @param program with DecreasesExp.
    * @return verifiable program (i.e. without DecreasesExp)
    */
  private def extractDecreasesExp(program: Program): (Program, Map[Function, DecreasesExp], Map[String, DecreasesExp]) = {
    val functionDecreaseMap = scala.collection.mutable.Map[Function, DecreasesExp]()
    val methodDecreaseMap = scala.collection.mutable.Map[String, DecreasesExp]()

    val result: Program = ViperStrategy.Slim({
      case f: Function =>
        val (decreases, posts) = f.posts.partition(p => p.isInstanceOf[DecreasesExp])

        if (decreases.nonEmpty) {
          // one DecreasesExp found
          val newFunction =
            Function(name = f.name, formalArgs = f.formalArgs, typ = f.typ, pres = f.pres, posts = posts, body = f.body)(f.pos, f.info, f.errT)

          if (decreases.nonEmpty) {
            functionDecreaseMap += (newFunction -> decreases.head.asInstanceOf[DecreasesExp])
          }
          newFunction
        } else {
          // none decreases clause
          f
        }
      case m: Method =>
        val (decreases, posts) = m.posts.partition(p => p.isInstanceOf[DecreasesExp])

        if (decreases.nonEmpty) {
          // one DecreasesExp found
          val newMethod =
            m.copy(posts = posts)(m.pos, m.info, m.errT)

          if (decreases.nonEmpty) {
            methodDecreaseMap += (newMethod.name -> decreases.head.asInstanceOf[DecreasesExp])
          }
          newMethod
        } else {
          // none decreases clause
          m
        }
    }).execute(program)
    (result, functionDecreaseMap.toMap, methodDecreaseMap.toMap)
  }
}