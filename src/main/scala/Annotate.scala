package bucharestfp

case class UnboundIdentifier(name: String) extends RuntimeException(s"unbound identifier: $name")

object Annotate {
  import Absyn._

  def annotate(absyn: Absyn, tenv: TypeEnv): Tysyn = {
    absyn match {
      case INT(value) => Tysyn.INT(Type.TINT, value)
      case ADD(a, b) => Tysyn.ADD(Type.TINT, annotate(a, tenv), annotate(b, tenv))
      case SUB(a, b) => Tysyn.SUB(Type.TINT, annotate(a, tenv), annotate(b, tenv))
      case BOOL(value) => Tysyn.BOOL(Type.TBOOL, value)
      case IF(test, yes, no) =>
        Tysyn.IF(Type.freshVar(), annotate(test, tenv), annotate(yes, tenv), annotate(no, tenv))
      case APP(fn, arg) =>
        Tysyn.APP(Type.freshVar(), annotate(fn, tenv), annotate(arg, tenv))
      case FN(param, body) =>
        val paramType = Type.freshVar()
        val extendedTenv = tenv.set(param, paramType)
        val annotatedBody = annotate(body, extendedTenv)
        val bodyType = annotatedBody.ty
        val fnType = Type.TFN(paramType, bodyType)
        Tysyn.FN(fnType, param, annotatedBody)
      case VAR(name) =>
        tenv.lookup(name) match {
          case None => throw UnboundIdentifier(name)
          case Some(ty) => Tysyn.VAR(ty, name)
        }
      case LET(binding, value, body) =>
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
    tysyn match {
      case INT(_, _) => List.empty
      case ADD(_, a, b) =>
        List(
          Constraint(a.ty, Type.TINT),
          Constraint(b.ty, Type.TINT)
        ) ++ constrain(a) ++ constrain(b)
      case SUB(_, a, b) =>
        List(
          Constraint(a.ty, Type.TINT),
          Constraint(b.ty, Type.TINT)
        ) ++ constrain(a) ++ constrain(b)
      case BOOL(_, _) => List.empty
      case VAR(_, _) => List.empty
      case FN(_, _, body) => constrain(body)
      case IF(ty, test, yes, no) =>
        List(
          Constraint(ty, yes.ty),
          Constraint(test.ty, Type.TBOOL),
          Constraint(yes.ty, no.ty)
        ) ++ constrain(test) ++ constrain(yes) ++ constrain(no)
      case APP(ty, fn, arg) =>
        Constraint(fn.ty, Type.TFN(arg.ty, ty)) :: constrain(fn) ++ constrain(arg)
      case LET(ty, _, value, body) =>
        Constraint(ty, body.ty) :: constrain(value) ++ constrain(body)
    }
  }
}
