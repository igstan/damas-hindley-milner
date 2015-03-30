package bucharestfp
package parser

object LatexCompiler {
  def compile(args: Array[String]): Unit = {
    val absyn = Parser.parse(Scanner.scan("""
      let
        val const = fn a => fn b => a
      in
        const 1 2
      end
    """))

    // val latex = compileAbsynTree(absyn)
    val latex = compileTysynTree(Annotate.annotate(absyn, TypeEnv.empty))

    println(latex)
  }

  def compileAbsynTree(absyn: Absyn): String = {
    s"\\Tree ${compileAbsyn(absyn)}"
  }

  def compileTysynTree(tysyn: Tysyn): String = {
    s"\\Tree ${compileTysyn(tysyn)}"
  }

  def compileAbsyn(absyn: Absyn): String = {
    import Absyn._

    absyn match {
      case INT(value) =>
        s"[.INT ${value.toString} ]"
      case ADD(a, b) =>
        val treeA = compileAbsyn(a)
        val treeB = compileAbsyn(b)
        s"[.ADD $treeA $treeB ]"
      case SUB(a, b) =>
        val treeA = compileAbsyn(a)
        val treeB = compileAbsyn(b)
        s"[.SUB $treeA $treeB ]"
      case BOOL(value) =>
        s"[.BOOL ${value.toString} ]"
      case VAR(name) =>
        s"[.VAR $name ]"
      case IF(test, yes, no) =>
        val treeTest = compileAbsyn(test)
        val treeYes = compileAbsyn(yes)
        val treeNo = compileAbsyn(no)
        s"[.IF $treeTest $treeYes $treeNo ]"
      case FN(param, body) =>
        val treeBody = compileAbsyn(body)
        s"[.FN $param $treeBody ]"
      case APP(fn, arg) =>
        val treeFn = compileAbsyn(fn)
        val treeArg = compileAbsyn(arg)
        s"[.APP $treeFn $treeArg ]"
      case LET(binding, value, body) =>
        val treeValue = compileAbsyn(value)
        val treeBody = compileAbsyn(body)
        s"[.LET $binding $treeValue $treeBody ]"
    }
  }

  def compileTysyn(tysyn: Tysyn): String = {
    import Tysyn._

    def subscript(ty: Type): String = {
      ty match {
        case TINT => "{int}"
        case TBOOL => "{bool}"
        case TFN(paramTy, returnTy) => "{" + subscript(paramTy) + "} \\rightarrow " + subscript(returnTy)
        case TVAR(index) => "{t_" + index.toString + "}"
      }
    }

    def style(ty: Type, node: String): String = {
      "${\\texttt{\\color{blue}" + node + "}}_{\\color{red}"+ subscript(ty) +"}$"
    }

    tysyn match {
      case INT(ty, value) =>
        s"""[.${style(ty, "INT")} ${value.toString} ]"""
      case ADD(ty, a, b) =>
        val treeA = compileTysyn(a)
        val treeB = compileTysyn(b)
        s"""[.${style(ty, "ADD")} $treeA $treeB ]"""
      case SUB(ty, a, b) =>
        val treeA = compileTysyn(a)
        val treeB = compileTysyn(b)
        s"""[.${style(ty, "SUB")} $treeA $treeB ]"""
      case BOOL(ty, value) =>
        s"""[.${style(ty, "BOOL")} ${value.toString} ]"""
      case VAR(ty, name) =>
        s"""[.${style(ty, "VAR")} $name ]"""
      case IF(ty, test, yes, no) =>
        val treeTest = compileTysyn(test)
        val treeYes = compileTysyn(yes)
        val treeNo = compileTysyn(no)
        s"""[.${style(ty, "IF")} $treeTest $treeYes $treeNo ]"""
      case FN(ty, param, body) =>
        val treeBody = compileTysyn(body)
        s"""[.${style(ty, "FN")} $param $treeBody ]"""
      case APP(ty, fn, arg) =>
        val treeFn = compileTysyn(fn)
        val treeArg = compileTysyn(arg)
        s"""[.${style(ty, "APP")} $treeFn $treeArg ]"""
      case LET(ty, binding, value, body) =>
        val treeValue = compileTysyn(value)
        val treeBody = compileTysyn(body)
        s"""[.${style(ty, "LET")} $binding $treeValue $treeBody ]"""
    }
  }
}
