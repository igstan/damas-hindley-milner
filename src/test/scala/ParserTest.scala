package bucharestfp
package test

import parser.{ Parser, Scanner }
import Absyn._

class ParserTest extends FunSuite with Matchers {
  test("sample parsing") {
    val tokens = Parser.parse(Scanner.scan("""
      let
        val arith = fn a => fn b => fn c => a + b + c
      in
        arith 1 2 (if true then 3 else 4)
      end
    """))

    tokens should be {
      LET(
        "arith",
        FN("a",
          FN("b",
            FN("c",
              ADD(
                VAR("a"),
                ADD(
                  VAR("b"),
                  VAR("c")
                )
              )
            )
          )
        ),
        APP(
          APP(
            APP(
              VAR("arith"),
              INT(1)
            ),
            INT(2)
          ),
          IF(
            BOOL(true),
            INT(3),
            INT(4)
          )
        )
      )
    }
  }
}
