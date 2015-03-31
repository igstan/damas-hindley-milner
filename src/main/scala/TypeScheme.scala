package bucharestfp

case class TypeScheme(tvars: Set[Type.Var], ty: Type) {
  def instantiate: Type = {
    tvars.foldLeft(ty) { (ty, tvar) =>
      ty.substitute(tvar, Type.freshVar())
    }
  }

  def freeVars: Set[Type.Var] = {
    ty.freeVars -- tvars
  }
}

object TypeScheme {
  def forall(tyvars: Set[Type.Var], ty: Type) = TypeScheme(tyvars, ty)
}
