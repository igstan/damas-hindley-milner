package bucharestfp

case class Substitution(records: List[(Type.Var, Type)]) {
  import Type._

  def apply(ty: Type): Type = {
    records.foldLeft(ty) { case (ty, (tvar, replacement)) =>
      ty.substitute(tvar, replacement)
    }
  }

  def compose(other: Substitution): Substitution = {
    Substitution(records ++ other.records)
  }
}

object Substitution {
  def empty = Substitution(List.empty)
}
