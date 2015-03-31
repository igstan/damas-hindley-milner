package bucharestfp

object Infer {
  def typeOf(absyn: Absyn, typeEnv: TypeEnv = TypeEnv.empty): TypeScheme = {
    Type.resetFreshness()
    val tysyn = Annotate.annotate(absyn, typeEnv)
    val constraints = Constrain.constrain(tysyn)
    val substitution = Unifier.unify(constraints)
    typeEnv.generalize(substitution.apply(tysyn.ty))
  }
}
