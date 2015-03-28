# Damas-Hindley-Milner

## Grammar

```bnf
<EXPR> ::= <INT>
         | <BOOL>
         | <IDENT>
         | "if" <EXPR> "then" <EXPR> "else" <EXPR>
         | "fn" <IDENT> "=>" <EXPR>
         | <EXPR> <EXPR>
         | "let" <BIND> "in" <EXPR> "end"

<BIND> ::= "val" <IDENT> "=" <EXPR>

<INT> ::= <DIGITS>

<DIGITS> ::= <DIGIT> <DIGITS>

<DIGIT> ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"

<BOOL> ::= "true"
         | "false"

<IDENT> ::= <LETTER> <LETTERS>

<LETTER> ::= "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k"
           | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v"
           | "w" | "x" | "y" | "z"

<LETTERS> ::= ""
            | <LETTER> <LETTERS>
```
