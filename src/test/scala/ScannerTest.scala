package bucharestfp
package test

import parser.{ Parser, Scanner, Token }, Token._

class LexerTest extends FunSuite with Matchers {
  test("sample scanning") {
    val tokens = Scanner.scan("""
      let
        val arith = fn a => fn b => fn c => a + b + c
      in
        arith 1 2 3
      end
    """)

    tokens should be(List(
      LET,
      VAL,
      VAR("arith"),
      EQUAL,
      FN,
      VAR("a"),
      DARROW,
      FN,
      VAR("b"),
      DARROW,
      FN,
      VAR("c"),
      DARROW,
      VAR("a"),
      ADD,
      VAR("b"),
      ADD,
      VAR("c"),
      IN,
      VAR("arith"),
      INT(1),
      INT(2),
      INT(3),
      END
    ))
  }
}
