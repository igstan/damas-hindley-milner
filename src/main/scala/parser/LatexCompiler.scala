package bucharestfp
package parser

object LatexCompiler {
  def main(args: Array[String]): Unit = {
    val latex = compileAbsyn(Parser.parse(Scanner.scan("""
      let
        val arith = fn a => fn b => fn c => a + b + c
      in
        arith 1 2 (if true then 3 else 4)
      end
    """)))

    println(latex)
  }

  // \Tree [.LET id
  //           [.FN a [.VAR a ] ]
  //           [.APP [.VAR id ] [.INT 42 ] ] ]
  def compileAbsyn(absyn: Absyn): String = {
    s"\\Tree ${compileTree(absyn)}"
  }

  def compileTree(absyn: Absyn): String = {
    import Absyn._

    absyn match {
      case INT(value) =>
        s"[.INT ${value.toString} ]"
      case ADD(a, b) =>
        val treeA = compileTree(a)
        val treeB = compileTree(a)
        s"[.ADD $treeA $treeB ]"
      case SUB(a, b) =>
        val treeA = compileTree(a)
        val treeB = compileTree(a)
        s"[.SUB $treeA $treeB ]"
      case BOOL(value) =>
        s"[.BOOL ${value.toString} ]"
      case VAR(name) =>
        s"[.VAR $name ]"
      case IF(test, yes, no) =>
        val treeTest = compileTree(test)
        val treeYes = compileTree(yes)
        val treeNo = compileTree(no)
        s"[.IF $treeTest $treeYes $treeNo ]"
      case FN(param, body) =>
        val treeBody = compileTree(body)
        s"[.FN $param $treeBody ]"
      case APP(fn, arg) =>
        val treeFn = compileTree(fn)
        val treeArg = compileTree(arg)
        s"[.APP $treeFn $treeArg ]"
      case LET(binding, value, body) =>
        val treeValue = compileTree(value)
        val treeBody = compileTree(body)
        s"[.LET $binding $treeValue $treeBody ]"
    }
  }
}
