package bucharestfp

case class UnboundIdentifier(name: String) extends RuntimeException(s"unbound identifier: $name")

object Annotate {
  def annotate(absyn: Absyn, tenv: TypeEnv): Tysyn = {
    absyn match {
      case Absyn.INT(value) => Tysyn.INT(Type.INT, value)
      case Absyn.ADD(a, b) => Tysyn.ADD(Type.FN(Type.INT, Type.INT), annotate(a, tenv), annotate(b, tenv))
      case Absyn.SUB(a, b) => Tysyn.SUB(Type.FN(Type.INT, Type.INT), annotate(a, tenv), annotate(b, tenv))
      case Absyn.BOOL(value) => Tysyn.BOOL(Type.BOOL, value)
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
        val fnType = Type.FN(paramType, bodyType)
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
  def constrain(tysyn: Tysyn): List[Constraint] = {
    ???
  }
}
