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
      IDENT("arith"),
      EQUAL,
      FN,
      IDENT("a"),
      DARROW,
      FN,
      IDENT("b"),
      DARROW,
      FN,
      IDENT("c"),
      DARROW,
      IDENT("a"),
      PLUS,
      IDENT("b"),
      PLUS,
      IDENT("c"),
      IN,
      IDENT("arith"),
      NUMBER(1),
      NUMBER(2),
      NUMBER(3),
      END
    ))
  }
}
