package bucharestfp

import parser.{ Parser, Scanner }

object Main {
  def main(args: Array[String]): Unit = {
    val tokens = Scanner.scan("""
      let
        val id = fn a => a
      in
        id 1
      end
    """)

    println(s"tokens: $tokens")

    val absyn = Parser.parse(tokens)

    println(s"absyn: $absyn")
  }
}
