package jsy.student

import jsy.lab5.Lab5Like

object Lab5 extends jsy.util.JsyApplication with Lab5Like {
  import jsy.lab5.ast._
  import jsy.util.DoWith
  import jsy.util.DoWith._

  /*
   * CSCI 3155: Lab 5
   * <Your Name>
   *
   * Partner: <Your Partner's Name>
   * Collaborators: <Any Collaborators>
   */

  /*
   * Fill in the appropriate portions above by replacing things delimited
   * by '<'... '>'.
   *
   * Replace the '???' expression with your code in each function.
   *
   * Do not make other modifications to this template, such as
   * - adding "extends App" or "extends Application" to your Lab object,
   * - adding a "main" method, and
   * - leaving any failing asserts.
   *
   * Your lab will not be graded if it does not compile.
   *
   * This template compiles without error. Before you submit comment out any
   * code that does not compile or causes a failing assert. Simply put in a
   * '???' as needed to get something that compiles without error. The '???'
   * is a Scala expression that throws the exception scala.NotImplementedError.
   */

  /*** Exercise with DoWith ***/

  def rename[W](env: Map[String,String], e: Expr)(fresh: String => DoWith[W,String]): DoWith[W,Expr] = {
    def ren(env: Map[String,String], e: Expr): DoWith[W,Expr] = e match {
      case N(_) | B(_) | Undefined | S(_) | Null | A(_) => doreturn(e)
      case Print(e1) => ren(env,e1) map { e1p => Print(e1p) }

      case Unary(uop, e1) => ren(env, e1) map {e1p => Unary(uop, e1p)}
      case Binary(bop, e1, e2) =>
        ren(env, e1) flatMap  { e1p =>
          ren(env, e2) map { e2p =>
            Binary(bop, e1p, e2p)
          }
        }
      case If(e1, e2, e3) => ren(env, e1) flatMap { e1p => ren(env, e2) flatMap { e2p => ren(env, e3) map { e3p => If(e1p, e2p, e3p) } } }

      case Var(x) => doreturn(Var(env.getOrElse(x,x)))

      case Decl(m, x, e1, e2) => fresh(x) flatMap { xp =>
        val envex = extend(env, x, xp)
        ren(env, e1) flatMap {
          e1p => ren(envex, e2) map {
            e2p => Decl(m, xp, e1p, e2p)
          }
        }
      }
      case Function(p, params, retty, e1) => {
        val w: DoWith[W, (Option[String], Map[String, String])] = p match {
          case None => doreturn((None, env))
          case Some(x) => fresh(x) map { xp => (Some(xp), extend(env, x, xp)) }
        }
        w flatMap { case (pp, envp) =>
          params.foldRight[DoWith[W, (List[(String, MTyp)], Map[String, String])]](doreturn((Nil, envp))) {
            case ((x, mty), acc) => acc flatMap {
              accp => {
                fresh(x) map { xp => ((xp, mty) :: accp._1, extend(accp._2, x, xp)) }
              }
            }
          } flatMap {
            accp => ren(accp._2, e1) map { e1p => Function(pp, accp._1, retty, e1p) }
          }
        }
      }
      case Call(e1, args) => ren(env, e1) flatMap {
        e1p => mapWith(args) {
          arg => ren(env, arg)
        } map {
          argsp => Call(e1p, argsp)
        }
      }

      case Obj(fields) => mapWith(fields) { case (f,e) => ren(env, e) map {ep => (f, ep) } } map { e2p => Obj(e2p)}
      case GetField(e1, f) => ren(env, e1) map { e1p => GetField(e1p, f) }

      case Assign(e1, e2) => ren(env, e1) flatMap { e1p => ren(env, e2) map {e2p => Assign(e1p, e2p) }}
      /* Should not match: should have been removed */
      case InterfaceDecl(_, _, _) => throw new IllegalArgumentException("Gremlins: Encountered unexpected expression %s.".format(e))
    }
    ren(env, e)


  }

  def myuniquify(e: Expr): Expr = {
    val fresh: String => DoWith[Int,String] = { _ =>
      doget flatMap { i =>
        doput(i + 1) map (_ => "x" + i)
      }
    }
    val (_, r) = rename(empty, e)(fresh)(0)
    r
  }

  /*** Helper: mapFirst to DoWith ***/

  // List map with an operator returning a DoWith
  def mapWith[W,A,B](l: List[A])(f: A => DoWith[W,B]): DoWith[W,List[B]] = {
    l.foldRight[DoWith[W,List[B]]]( doreturn(Nil) ) {
      case(a,dwb)=>dwb.flatMap((bs)=>f(a).map( (b) => b::bs))
    }
  }

  // Map map with an operator returning a DoWith
  def mapWith[W,A,B,C,D](m: Map[A,B])(f: ((A,B)) => DoWith[W,(C,D)]): DoWith[W,Map[C,D]] = {
    m.foldRight[DoWith[W,Map[C,D]]]( ??? ) {
      ???
    }
  }

  // Just like mapFirst from Lab 4 but uses a callback f that returns a DoWith in the Some case.
  def mapFirstWith[W,A](l: List[A])(f: A => Option[DoWith[W,A]]): DoWith[W,List[A]] = l match {
    case Nil => doreturn(l)
    case h :: t => f(h) match {
      case None => {
        mapFirstWith(t)(f) map {
          x => List(h) ::: x
        }
      }
      case Some(withhp) => {
        withhp map {
          a => List(a) ::: t
        }
      }
    }
  }

  // There are better ways to deal with the combination of data structures like List, Map, and
  // DoWith, but we won't tackle that in this assignment.

  /*** Casting ***/

  def castOk(t1: Typ, t2: Typ): Boolean = (t1, t2) match {
      /***** Make sure to replace the case _ => ???. */
    //case _ => ???
    case (TNull,TObj(_))=>true
    case (_,_)=> if(t1==t2) true else false
    case (TObj(f1),TObj(f2))=> {
      f1.forall{
        case(a,b) if(b==None)=> true
        case (a,b)=> f2.get(a) match {
          case None => true
          case Some(x)=> castOk(b,x)
          
        }
      }
    }
      /***** Cases for the extra credit. Do not attempt until the rest of the assignment is complete. */
    case (TInterface(tvar, t1p), _) => ???
    case (_, TInterface(tvar, t2p)) => ???
      /***** Otherwise, false. */
    case _ => false
  }

  /*** Type Inference ***/

  // A helper function to check whether a jsy type has a function type in it.
  // While this is completely given, this function is worth studying to see
  // how library functions are used.
  def hasFunctionTyp(t: Typ): Boolean = t match {
    case TFunction(_, _) => true
    case TObj(fields) if (fields exists { case (_, t) => hasFunctionTyp(t) }) => true
    case _ => false
  }

  def isBindex(m: Mode, e: Expr): Boolean = ???

  def typeof(env: TEnv, e: Expr): Typ = {
    def err[T](tgot: Typ, e1: Expr): T = throw StaticTypeError(tgot, e1, e)

    e match {
      case Print(e1) => typeof(env, e1); TUndefined
      case N(_) => TNumber
      case B(_) => TBool
      case Undefined => TUndefined
      case S(_) => TString
    //  case Var(x) if(env(x))=> env(x)._1._2
      case Unary(Neg, e1) => typeof(env, e1) match {
        case TNumber => TNumber
        case tgot => err(tgot, e1)
      }
        /***** Cases directly from Lab 4. We will minimize the test of these cases in Lab 5. */
      case Unary(Not, e1) => typeof(env,e1) match {
        case TBool => TBool
        case tgot => err(tgot, e1)
      }
      case Binary(Plus, e1, e2) => typeof(env,e1) match {
        case TNumber => typeof(env,e2) match {
          case TNumber => TNumber
          case tgot => err(tgot, e2)
        }
        case TString => typeof(env,e2) match {
          case TString => TString
          case tgot => err(tgot, e2)
        }
        case tgot => err(tgot, e1)
      }
      case Binary(Minus|Times|Div, e1, e2) => typeof(env,e1) match {
        case TNumber => typeof(env,e2) match {
          case TNumber => TNumber
          case tgot => err(tgot, e2)
        }
        case tgot => err(tgot, e1)
      }
      case Binary(Eq|Ne, e1, e2) => typeof(env,e1) match {
        case t1 if !hasFunctionTyp(t1) => typeof(env,e2) match {
          case t2 if (t1 == t2) => TBool
          case tgot => err(tgot, e2)
        }
        case tgot => err(tgot, e1)
      }
      case Binary(Lt|Le|Gt|Ge, e1, e2) =>typeof(env,e1) match {
        case TNumber => typeof(env,e2) match {
          case TNumber => TBool
          case tgot => err(tgot, e2)
        }
        case TString => typeof(env,e2) match {
          case TString => TBool
          case tgot => err(tgot, e2)
        }
        case tgot => err(tgot, e1)
      }
      case Binary(And|Or, e1, e2) =>typeof(env,e1) match {
        case TBool => typeof(env,e2) match {
          case TBool => TBool
          case tgot => err(tgot, e2)
        }
        case tgot => err(tgot, e1)
      }
      case Binary(Seq, e1, e2) =>typeof(env,e1);typeof(env,e2)
      case If(e1, e2, e3) =>typeof(env,e1) match {
        case TBool =>
          val (t2, t3) = (typeof(env,e2), typeof(env,e3))
          if (t2 == t3) t2 else err(t3, e3)
        case tgot => err(tgot, e1)
      }

      case Obj(fields) => TObj(fields map { case (f,t) => (f, typeof(env,t)) })
      case GetField(e1, f) =>typeof(env,e1) match {
        case TObj(tfields) if (tfields.contains(f)) => tfields(f)
        case tgot => err(tgot, e1)
      }

        /***** Cases from Lab 4 that need a small amount of adapting. */
      case Decl(m, x, e1, e2) => ??? //typeof(extend(env,x,typeof(env,e1)), e2)

      case Function(p, params, tann, e1) => {
        // Bind to env1 an environment that extends env with an appropriate binding if
        // the function is potentially recursive.
        val env1 = (p, tann) match {
          case (Some(f), Some(tret)) =>
            val tprime = TFunction(params, tret)
            env + (f -> (MConst, tprime))
          case (None, _) => env
          case _ => err(TUndefined, e1)
        }
        // Bind to env2 an environment that extends env1 with bindings for params.
        val env2 = params match {
//          case Left(params) => params.foldLeft(env1) {
//            case (env, param) => param match {
//              case(a, b) => env + (a -> (MConst, b))
//            }
          case _=> ???
        }
        // Match on whether the return type is specified.
        tann match {
          case None => ???
          case Some(tret) => ???
        }
      }
      case Call(e1, args) => typeof(env, e1) match {
        case TFunction(params, tret) if (params.length == args.length) =>
          (params, args).zipped.foreach {
            case ((_, t), x) => if (t != typeof(env,x)) err(typeof(env,x), x)
          }
          tret
//        case tgot @ TFunction(Right((mode, _, tparam)), tret) if (args.length == 1) => {
//          val targ0 = typeof(env,args(0))
//          mode match {
//            case MName | MVar => {
//              if (targ0 != tparam) {
//                err(targ0, args(0))
//              } else {
//                tret
//              }
//            }
//            case MRef if isLExpr(args(0)) => {
//              if (targ0 != tparam) {
//                err(targ0, args(0))
//              } else {
//                tret
//              }
//            }
//            case _ => err(tgot, e1)
//          }
//        }
        case tgot => err(tgot, e1)
      }

        /***** New cases for Lab 5. ***/
//      case Assign(Var(x), e1) if (env(x)._1 == MVar) => {
//        val t1 = typeof(env,Var(x))
//         if (t1 == typeof(env,e1)) t1 else err(t1, Var(x))
//    }
      case Assign(GetField(e1, f), e2) => typeof(env,e1) match {
        case TObj(tfields) if (tfields.contains(f)) => {
          val t2 = typeof(env,e2)
          if (tfields(f) == t2) t2 else err(t2, e2)
          //case tgot => err(tgot, e1)
        }
        case tgot => err(typeof(env,e1), e1)
    }
      case Assign(_, _) => err(TUndefined, e)

      case Null =>TNull

      case Unary(Cast(t), e1) => typeof(env, e1) match {
        case tgot if(castOk(typeof(env,e1),t)) => t

        case tgot => err(tgot, e1)
      }

      /* Should not match: non-source expressions or should have been removed */
      case A(_) | Unary(Deref, _) | InterfaceDecl(_, _, _) => throw new IllegalArgumentException("Gremlins: Encountered unexpected expression %s.".format(e))
    }
  }

  /*** Small-Step Interpreter ***/

  /*
   * Helper function that implements the semantics of inequality
   * operators Lt, Le, Gt, and Ge on values.
   *
   * We suggest a refactoring of code from Lab 2 to be able to
   * use this helper function in eval and step.
   *
   * This should the same code as from Lab 3 and Lab 4.
   */
  def inequalityVal(bop: Bop, v1: Expr, v2: Expr): Boolean = {
    require(isValue(v1), s"inequalityVal: v1 ${v1} is not a value")
    require(isValue(v2), s"inequalityVal: v2 ${v2} is not a value")
    require(bop == Lt || bop == Le || bop == Gt || bop == Ge)
    (v1, v2) match {
      case (S(s1), S(s2)) =>
        (bop: @unchecked) match {
          case Lt => s1 < s2
          case Le => s1 <= s2
          case Gt => s1 > s2
          case Ge => s1 >= s2
        }
      case (N(n1), N(n2)) =>
        (bop: @unchecked) match {
          case Lt => n1 < n2
          case Le => n1 <= n2
          case Gt => n1 > n2
          case Ge => n1 >= n2
        }
      case _ => ???
    }
  }

  /* Capture-avoiding substitution in e replacing variables x with esub. */
  def substitute(e: Expr, esub: Expr, x: String): Expr = {
    def subst(e: Expr): Expr = e match {
      case N(_) | B(_) | Undefined | S(_) | Null | A(_) => e
      case Print(e1) => Print(subst(e1))
        /***** Cases from Lab 3 */
      case Unary(uop, e1) => Unary(uop,subst(e1))
      case Binary(bop, e1, e2) => Binary(bop,subst(e1),subst(e2))
      case If(e1, e2, e3) => If(subst(e1),subst(e2),subst(e3))
      case Var(y) => if (x == y) esub else e
        /***** Cases need a small adaption from Lab 3 */
      case Decl(mut, y, e1, e2) => Decl(mut, y, subst(e1), if (x == y) e2 else subst(e2))
        /***** Cases needing adapting from Lab 4 */
      case Function(p, paramse, retty, e1) => ??? /*paramse match {
        case Left(params) => {
          val e1p = params.foldLeft(e1) {
            (e1, param) => param match {
              case (pname, ptype) => if (pname != x && p != Some(x)) {
                subst(e1)
              } else {
                e1
              }
            }
          }
          Function(p, Left(params), retty, e1p)
        }
        case Right((pmode, pname, ptype)) => {
          val e1p = {
            if (pname != x && p != Some(x)) {
              subst(e1)
            } else {
              e1
            }
          }
          Function(p, Right(pmode, pname, ptype), retty, e1p)
        }
      }
      }*/

        /***** Cases directly from Lab 4 */
      case Call(e1, args) =>  Call(subst(e1), args map subst)
      case Obj(fields) => Obj(fields map { case (fi,ei) => (fi, subst(ei)) })
      case GetField(e1, f) =>  GetField(subst(e1), f)
        /***** New case for Lab 5 */
      case Assign(e1, e2) => Assign(subst(e1), subst(e2))

      /* Should not match: should have been removed */
      case InterfaceDecl(_, _, _) => throw new IllegalArgumentException("Gremlins: Encountered unexpected expression %s.".format(e))
    }

    def myrename(e: Expr): Expr = {
      val fvs = freeVars(esub)
      def fresh(x: String): String = if (fvs contains x) fresh(x + "$") else x
      rename[Unit](e)(???){ x => ??? }
    }

    subst(???)
  }

  /* Check whether or not an expression is reduced enough to be applied given a mode. */
  def isRedex(mode: Mode, e: Expr): Boolean = ???

  def getBinding(mode: Mode, e: Expr): DoWith[Mem,Expr] = {
    require(!isRedex(mode,e), s"expression ${e} must not reducible under mode ${mode}")
    ???
  }

  /* A small-step transition. */
  def step(e: Expr): DoWith[Mem, Expr] = {
    require(!isValue(e), "stepping on a value: %s".format(e))
    e match {
      /* Base Cases: Do Rules */
      case Print(v1) if isValue(v1) => doget map { m => println(pretty(m, v1)); Undefined }
        /***** Cases needing adapting from Lab 3. */
      case Unary(Neg, N(n1)) =>  doreturn( N(- n1))
      /***** More cases here */
      case Unary(Not, B(b1)) => doreturn( B(! b1) )
      case Binary(Seq, v1, e2) if isValue(v1) => doreturn( e2 )
      case Binary(Plus, S(s1), S(s2)) => doreturn( S(s1 + s2) )
      case Binary(Plus, N(n1), N(n2)) => doreturn( N(n1 + n2) )
      case Binary(bop @ (Lt|Le|Gt|Ge), v1, v2) if isValue(v1) && isValue(v2) => doreturn( B(inequalityVal(bop, v1, v2)) )
      case Binary(Eq, v1, v2) if isValue(v1) && isValue(v2) => doreturn( B(v1 == v2) )
      case Binary(Ne, v1, v2) if isValue(v1) && isValue(v2) => doreturn( B(v1 != v2) )
      case Binary(And, B(b1), e2) => doreturn( if (b1) e2 else B(false) )
      case Binary(Or, B(b1), e2) => doreturn( if (b1) B(true) else e2 )
      case Binary(Minus, N(n1), N(n2)) => doreturn( N(n1 - n2) )
      case Binary(Times, N(n1), N(n2)) => doreturn( N(n1 * n2) )
      case Binary(Div, N(n1), N(n2)) => doreturn( N(n1 / n2) )
      case If(B(b1), e2, e3) => doreturn( if (b1) e2 else e3 )
        /***** Cases needing adapting from Lab 4. */
      case Obj(fields) if (fields forall { case (_, vi) => isValue(vi)}) => ???
//        Mem.alloc(Obj(fields)) map {
//          (a:A) => a:Expr
//        }
      case GetField(a @ A(_), f) =>{
        doget.map {
          (m: Mem) => m.get(a) match {
            case Some(Obj(fields)) => fields.get(f) match {
              case Some(field) => field
              case _ => throw StuckError(e)
            }
            case _ => throw StuckError(e)
          }
        }

      }


      case Decl(MConst, x, v1, e2) if isValue(v1) => doreturn(substitute(e2, v1, x))
      case Decl(MVar, x, v1, e2) if isValue(v1) =>{
//        Mem.alloc(v1) map {
//          a => substitute(e2, Unary(Deref, a), x)
//        }
        ???
      }


        /***** New cases for Lab 5. */
      case Unary(Deref, a @ A(_)) =>{
        doget.map {
          (m: Mem) => m.apply(a)
        }
      }

      case Assign(Unary(Deref, a @ A(_)), v) if isValue(v) =>
        domodify[Mem] { m => m+ (a -> v) } map { _ => v }

      case Assign(GetField(a @ A(_), f), v) if isValue(v) =>{
        domodify {
          (m: Mem) => m(a) match {
            case Obj(fields) => {
              m + (a -> Obj(fields + ((f, v))))
            }
            case _ => throw new StuckError(e)
          }
        } map {
          _ => v
        }
      }

      case Call(v @ Function(p, params, _, e), args) => {
        val pazip = params zip args
        if (???) {
          val dwep = pazip.foldRight( ??? : DoWith[Mem,Expr] )  {
            case (((xi, MTyp(mi, _)), ei), dwacc) => ???
          }
          p match {
            case None => ???
            case Some(x) => ???
          }
        }
        else {
          val dwpazipp = mapFirstWith(pazip) {
            ???
          }
          ???
        }
      }

      /* Base Cases: Error Rules */
        /***** Replace the following case with a case to throw NullDeferenceError.  */
      //case _ => throw NullDeferenceError(e)

      /* Inductive Cases: Search Rules */
        /***** Cases needing adapting from Lab 3. Make sure to replace the case _ => ???. */
      case Print(e1) => step(e1) map { e1p => Print(e1p) }
      case Unary(uop, e1) => step(e1) map {e1p => Unary(uop, e1p)}
      case Binary(bop, v1, e2) if isValue(v1) => step(e2) map {e2p => Binary(bop, v1, e2p)}
      case Binary(bop, e1, e2) => step(e1) map{e1p=> Binary(bop, e1p, e2)}
      case If(e1, e2, e3) => step(e1) map{e1p => If(e1p, e2, e3)}
        /***** Cases needing adapting from Lab 4 */
      case GetField(e1, f) =>{
        if (e1 == Null) throw new NullDereferenceError(e1)
        step(e1) map{e1p => GetField(e1p, f)}
      }
      case Obj(fields) =>  fields find { case (_, ei) => !isValue(ei) } match {
        case Some((fi, ei)) => {
          step(ei) map { eip => Obj(fields + (fi -> eip)) }
        }
      }

      case Decl(mode, x, e1, e2) => step(e1) map{ e1p=> Decl(mode, x, e1p, e2)}
      case Call(e1, args) => step(e1) map{ e1p=> Call(e1p, args)}

        /***** New cases for Lab 5.  */
      case Assign(e1, e2) if(isLValue(e1) && !isValue(e2)) => step(e2) map{ e2p=> Assign(e1, e2p)}

        case Assign(e1, e2) => step(e1) map{ e1p=> Assign(e1p, e2)}

      /* Everything else is a stuck error. */
      case _ => throw StuckError(e)
    }
  }

  /*** Extra Credit: Lowering: Remove Interface Declarations ***/

  def lower(e: Expr): Expr =
    /* Do nothing by default. Change to attempt extra credit. */
    e

  /*** External Interfaces ***/

  //this.debug = true // comment this out or set to false if you don't want print debugging information
  this.maxSteps = Some(1000) // comment this out or set to None to not bound the number of steps.
  this.keepGoing = true // comment this out if you want to stop at first exception when processing a file
}
