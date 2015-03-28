package bucharestfp
package parser

case class UnexpectedEndOfStream(expected: Token*)
  extends RuntimeException(s"""Unexpected end of stream; expected ${expected.mkString(", ")}.""")

case class ParseError(found: Token, expected: Token*)
  extends RuntimeException(s"""Unexpected token: "$found"; expected ${expected.mkString(", ")}.""")

object Parser {
  import Token._

  def parse(tokens: List[Token]): Absyn = {
    expr(tokens) match {
      case (absyn, Nil) => absyn
      case (_, token :: _) =>
        throw ParseError(token, NUMBER(0), TRUE, FALSE, IDENT("?"), IF, FN, LET)
    }
  }

  def expr(tokens: List[Token]): (Absyn, List[Token]) = {
    tokens match {
      case NUMBER(n) :: rest => Absyn.INT(n) -> rest
      case TRUE :: rest => Absyn.BOOL(true) -> rest
      case FALSE :: rest => Absyn.BOOL(false) -> rest
      case IDENT(v) :: rest => Absyn.VAR(v) -> rest
      case LPAREN :: tokens0 =>
        val (e, tokens1) = expr(tokens0)
        tokens1 match {
          case RPAREN :: token2 => e -> token2
          case token :: _ => throw ParseError(token, RPAREN)
          case _ => throw UnexpectedEndOfStream(expected = RPAREN)
        }
      case IF :: tokens0 =>
        val (testAbsyn, tokens1) = expr(tokens0)
        tokens1 match {
          case THEN :: tokens1 =>
            val (yesAbsyn, tokens2) = expr(tokens1)
            tokens2 match {
              case ELSE :: tokens2 =>
                val (noAbsyn, tokens3) = expr(tokens2)
                Absyn.IF(testAbsyn, yesAbsyn, noAbsyn) -> tokens3
              case token :: _ => throw ParseError(token, ELSE)
              case _ => throw UnexpectedEndOfStream(expected = ELSE)
            }
          case token :: _ => throw ParseError(token, THEN)
          case _ => throw UnexpectedEndOfStream(expected = THEN)
        }
      case FN :: param :: tokens0 =>
        param match {
          case IDENT(v) =>
            tokens0 match {
              case DARROW :: tokens1 =>
                val (bodyAbsyn, tokens2) = expr(tokens1)
                Absyn.FN(v, bodyAbsyn) -> tokens2
              case token :: _ => throw ParseError(token, DARROW)
              case _ => throw UnexpectedEndOfStream(expected = DARROW)
            }
          case token => throw ParseError(token, IDENT("?"))
        }
      case LET :: tokens0 =>
        tokens0 match {
          case VAL :: tokens1 =>
            tokens1 match {
              case IDENT(bindingName) :: tokens2 =>
                tokens2 match {
                  case EQUAL :: tokens3 =>
                    val (bindingAbsyn, tokens4) = expr(tokens3)
                    tokens4 match {
                      case IN :: tokens5 =>
                        val (bodyAbsyn, tokens6) = expr(tokens5)
                        tokens6 match {
                          case END :: tokens7 =>
                            Absyn.LET(bindingName, bindingAbsyn, bodyAbsyn) -> tokens7
                          case token :: _ => throw ParseError(token, END)
                          case _ => throw UnexpectedEndOfStream(expected = END)
                        }
                      case token :: _ => throw ParseError(token, IN)
                      case _ => throw UnexpectedEndOfStream(expected = IN)
                    }
                  case token :: _ => throw ParseError(token, EQUAL)
                  case _ => throw UnexpectedEndOfStream(expected = EQUAL)
                }
              case token :: _ => throw ParseError(token, IDENT("?"))
              case _ => throw UnexpectedEndOfStream(expected = IDENT("?"))
            }
          case token :: _ => throw ParseError(token, VAL)
          case _ => throw UnexpectedEndOfStream(expected = VAL)
        }
      case token :: _ => throw ParseError(token, NUMBER(0), TRUE, FALSE, IDENT("?"), IF, FN, LET)
      case _ => throw UnexpectedEndOfStream(NUMBER(0), TRUE, FALSE, IDENT("?"), IF, FN, LET)
    }
  }
}
