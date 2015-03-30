package bucharestfp

object Unifier {
  import Type._

  def unify(constraints: List[Constraint]): Substitution = {
    constraints match {
      case Nil => Substitution.empty
      case Constraint(ty1, ty2) :: tail =>
        val tailConstraints = unify(tail)
        val headConstraints = unifyPair(tailConstraints.apply(ty1), tailConstraints.apply(ty2))
        tailConstraints.compose(headConstraints)
    }
  }

  def unifyPair(a: Type, b: Type): Substitution = {
    (a, b) match {
      case (TINT, TINT) => Substitution.empty
      case (TBOOL, TBOOL) => Substitution.empty
      case (TFN(param1, return1), TFN(param2, return2)) =>
        // We can't generate these constraints in the constraint generation
        // phase because the two function types we have here may appear as a
        // result of unification itself, so the constraint generation phase
        // can't know about them.
        unify(List(
          Constraint(param1, param2),
          Constraint(return1, return2)
        ))
      case (TVAR(tvar), ty) => unifyVar(tvar, ty)
      case (ty, TVAR(tvar)) => unifyVar(tvar, ty)
      case _ => throw new RuntimeException(s"Cannot unify $a with $b")
    }
  }

  def unifyVar(tvar1: Type.Var, ty: Type): Substitution = {
    ty match {
      case TVAR(tvar2) if tvar1 == tvar2 => Substitution.empty
      case TVAR(_)                       => new Substitution(List(tvar1 -> ty))
      case ty if tvar1.occursIn(ty)      => throw new RuntimeException(s"Circular use: $tvar1 occurs in $ty")
      case ty                            => new Substitution(List(tvar1 -> ty))
    }
  }
}
