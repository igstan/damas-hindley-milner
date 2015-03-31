package bucharestfp

case class Substitution(records: List[(Type.Var, Type)]) {
  import Type._

  def apply(ty: Type): Type = {
    // records.foldLeft(ty) { case (ty, (tvar, replacement)) =>
    //   ty.substitute(tvar, replacement)
    // }
    def loop(ty: Type, records: List[(Type.Var, Type)]): Type = {
      records match {
        case Nil => ty
        case (tvar, replacement) :: rest =>
          loop(ty.substitute(tvar, replacement), rest)
      }
    }

    loop(ty, records)
  }

  def compose(other: Substitution): Substitution = {
    val subst = records.map { case (tvar, ty) => tvar -> other.apply(ty) }
    Substitution(subst ++ other.records)
  }
}

object Substitution {
  def empty = Substitution(List.empty)
}
