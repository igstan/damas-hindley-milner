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

    val ty = Infer.typeOf(Parser.parse(Scanner.scan(program)))

    println(s"ty: $ty")
  }
}
