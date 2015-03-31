package bucharestfp

case class Substitution(sulutions: List[(Type.Var, Type)]) {
  import Type._

  def apply(ty: Type): Type = {
    // sulutions.foldLeft(ty) { case (ty, (tvar, replacement)) =>
    //   ty.substitute(tvar, replacement)
    // }
    def loop(ty: Type, sulutions: List[(Type.Var, Type)]): Type = {
      sulutions match {
        case Nil => ty
        case (tvar, replacement) :: rest =>
          loop(ty.substitute(tvar, replacement), rest)
      }
    }

    loop(ty, sulutions)
  }

  def compose(other: Substitution): Substitution = {
    val subst = sulutions.map { case (tvar, ty) => tvar -> other.apply(ty) }
    Substitution(subst ++ other.sulutions)
  }
}

object Substitution {
  def empty = Substitution(List.empty)
}
