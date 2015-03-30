package bucharestfp

/**
 * Abstract syntax tree
 */
sealed trait Absyn
object Absyn {
  case class INT(value: Int) extends Absyn
  case class ADD(a: Absyn, b: Absyn) extends Absyn
  case class SUB(a: Absyn, b: Absyn) extends Absyn
  case class BOOL(value: Boolean) extends Absyn
  case class VAR(name: String) extends Absyn
  case class IF(test: Absyn, yes: Absyn, no: Absyn) extends Absyn
  case class FN(param: String, body: Absyn) extends Absyn
  case class APP(fn: Absyn, arg: Absyn) extends Absyn
  case class LET(binding: String, value: Absyn, body: Absyn) extends Absyn
}

sealed trait Type

case object TINT extends Type
case object TBOOL extends Type
case class TFN(paramTy: Type, returnTy: Type) extends Type
case class TVAR(index: Type.Var) extends Type

object Type {
  type Var = Int

  private var counter = -1

  def freshVar(): Type = {
    counter += 1
    TVAR(counter)
  }
}

/**
 * Typed syntax tree
 */
sealed trait Tysyn {
  def ty: Type
}

object Tysyn {
  case class INT(ty: Type, value: Int) extends Tysyn
  case class ADD(ty: Type, a: Tysyn, b: Tysyn) extends Tysyn
  case class SUB(ty: Type, a: Tysyn, b: Tysyn) extends Tysyn
  case class BOOL(ty: Type, value: Boolean) extends Tysyn
  case class VAR(ty: Type, name: String) extends Tysyn
  case class IF(ty: Type, test: Tysyn, yes: Tysyn, no: Tysyn) extends Tysyn
  case class FN(ty: Type, param: String, body: Tysyn) extends Tysyn
  case class APP(ty: Type, fn: Tysyn, arg: Tysyn) extends Tysyn
  case class LET(ty: Type, binding: String, value: Tysyn, body: Tysyn) extends Tysyn
}
