package bucharestfp
package test

import parser.Parser

class InferTest extends FunSuite with Matchers {
  test("increment function") {
    Infer.typeOf(Parser.parse("""
      let
        val inc = fn a => a + 1
      in
        inc 42
      end
    """)) should be {
      TINT
    }
  }

  test("constant function") {
    Infer.typeOf(Parser.parse("""
      fn a => fn b => a
    """)) should be {
      TFN(
        TVAR(Type.Var(3)),
        TFN(
          TVAR(Type.Var(4)),
          TVAR(Type.Var(3))
        )
      )
    }
  }

  test("higher-order function") {
    Infer.typeOf(Parser.parse("""
      fn pred => if pred 1 then 2 else 3
    """)) should be {
      TFN(TFN(TINT, TBOOL), TINT)
    }
  }
}
