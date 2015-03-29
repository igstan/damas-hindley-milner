package bucharestfp

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
