package bucharestfp

import parser.{ Parser, Scanner }

object Main {
  def main(args: Array[String]): Unit = {
    val tokens = Scanner.scan("""
      let
        val const = fn a => fn b => a
      in
        const 1 2
      end
    """)

    println(s"tokens: $tokens")

    val absyn = Parser.parse(tokens)

    println(s"absyn: $absyn")
  }
}
