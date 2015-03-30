package bucharestfp

case class UnboundIdentifier(name: String) extends RuntimeException(s"unbound identifier: $name")

object Annotate {
  def annotate(absyn: Absyn, tenv: TypeEnv): Tysyn = {
    absyn match {
      case Absyn.INT(value) => Tysyn.INT(Type.TINT, value)
      case Absyn.ADD(a, b) => Tysyn.ADD(Type.TINT, annotate(a, tenv), annotate(b, tenv))
      case Absyn.SUB(a, b) => Tysyn.SUB(Type.TINT, annotate(a, tenv), annotate(b, tenv))
      case Absyn.BOOL(value) => Tysyn.BOOL(Type.TBOOL, value)
      case Absyn.VAR(name) =>
        tenv.lookup(name) match {
          case None => throw UnboundIdentifier(name)
          case Some(ty) => Tysyn.VAR(ty, name)
        }
      case Absyn.IF(test, yes, no) =>
        Tysyn.IF(Type.freshVar(), annotate(test, tenv), annotate(yes, tenv), annotate(no, tenv))
      case Absyn.APP(fn, arg) =>
        Tysyn.APP(Type.freshVar(), annotate(fn, tenv), annotate(arg, tenv))
      case Absyn.FN(param, body) =>
        val paramType = Type.freshVar()
        val extendedTenv = tenv.set(param, paramType)
        val annotatedBody = annotate(body, extendedTenv)
        val bodyType = annotatedBody.ty
        val fnType = Type.TFN(paramType, bodyType)
        Tysyn.FN(fnType, param, annotatedBody)
      case Absyn.LET(binding, value, body) =>
        val annotatedValue = annotate(value, tenv)
        val extendedTenv = tenv.set(binding, annotatedValue.ty)
        val annotatedBody = annotate(body, extendedTenv)
        val letType = Type.freshVar()
        Tysyn.LET(letType, binding, annotatedValue, annotatedBody)
    }
  }
}

case class Constraint(a: Type, b: Type)

object Constrain {
  import Tysyn._

  def constrain(tysyn: Tysyn): List[Constraint] = {
    def loop(tysyns: List[Tysyn], constraints: List[Constraint]): List[Constraint] = {
      tysyns match {
        case Nil => constraints
        case tysyn :: rest =>
          tysyn match {
            case INT(_, _) => loop(rest, constraints)
            case ADD(_, a, b) =>
              val aConstr = Constraint(a.ty, Type.TINT)
              val bConstr = Constraint(b.ty, Type.TINT)
              loop(a :: b :: rest, aConstr :: bConstr :: constraints)
            case SUB(_, a, b) =>
              val aConstr = Constraint(a.ty, Type.TINT)
              val bConstr = Constraint(b.ty, Type.TINT)
              loop(a :: b :: rest, aConstr :: bConstr :: constraints)
            case BOOL(_, _) => loop(rest, constraints)
            case VAR(_, _) => loop(rest, constraints)
            case FN(_, _, body) => loop(body :: rest, constraints)
            case IF(ty, test, yes, no) =>
              val ifConstr = Constraint(ty, yes.ty)
              val testConstr = Constraint(test.ty, Type.TBOOL)
              val branchConstr = Constraint(yes.ty, no.ty)
              val newConstraints = ifConstr :: testConstr :: branchConstr :: constraints
              loop(test :: yes :: no :: rest, newConstraints)
            case APP(ty, fn, arg) =>
              val appConstr = Constraint(fn.ty, Type.TFN(arg.ty, ty))
              val newConstraints = appConstr :: constraints
              loop(fn :: arg :: rest, newConstraints)
            case LET(ty, binding, value, body) =>
              val letConstr = Constraint(ty, body.ty)
              val newConstraints = letConstr :: constraints
              loop(value :: body :: rest, newConstraints)
          }
      }
    }

    loop(List(tysyn), List.empty[Constraint])
  }
}
