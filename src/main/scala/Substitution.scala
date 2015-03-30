package bucharestfp

class Substitution(val records: List[(Type.Var, Type)]) {
  import Type._

  def apply(ty: Type): Type = {
    records.foldLeft(ty) { (ty, record) =>
      applyRecord(ty, record)
    }
  }

  def applyRecord(ty: Type, record: (Type.Var, Type)): Type = {
    ty match {
      case TINT => ty
      case TBOOL => ty
      case TFN(paramTy, returnTy) =>
        val substParamTy = applyRecord(paramTy, record)
        val substReturnTy = applyRecord(returnTy, record)
        TFN(substParamTy, substReturnTy)
      case TVAR(tVar1) =>
        val (tVar2, newTy) = record
        if (tVar1 == tVar2) newTy else ty
    }
  }

  def compose(other: Substitution): Substitution = {
    new Substitution(records ++ other.records)
  }
}

object Substitution {
  def empty = new Substitution(List.empty)
}
