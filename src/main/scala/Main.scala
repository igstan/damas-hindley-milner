package bucharestfp

import parser.Parser

object Main {
  def main(args: Array[String]): Unit = {
    println("typeOf(inc): " + Infer.typeOf(Parser.parse("""
      let
        val inc = fn a => a + 1
      in
        inc 42
      end
    """)))

    println("typeOf(const): " + Infer.typeOf(Parser.parse("""
      fn a => fn b => a
    """)))

    println("typeOf(pred): " + Infer.typeOf(Parser.parse("""
      fn pred => if pred 1 then 2 else 3
    """)))

    println("typeOf(compose): " + Infer.typeOf(Parser.parse("""
      fn f => fn g => fn x => f (g x)
    """)))
  }
}
