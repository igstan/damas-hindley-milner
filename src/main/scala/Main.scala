package bucharestfp

import parser.Scanner

object Main {
  def main(args: Array[String]): Unit = {
    val tokens = Scanner.scan("""
      let
        val a = 1
        val b = 2
      in
        fn b => a
      end
    """)

    println(s"tokens: $tokens")
  }
}
