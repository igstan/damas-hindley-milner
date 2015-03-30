package bucharestfp

import parser.{ Parser, Scanner }

object Main {
  def main(args: Array[String]): Unit = {
    val program = """
      let
        val inc = fn a => a + 1
      in
        inc 42
      end
    """

    val absyn = Parser.parse(Scanner.scan(program))
    val ty = Infer.typeOf(absyn)

    println(s"absyn: $absyn")
    println(s"ty: $ty")
  }
}
