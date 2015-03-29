# Damas-Hindley-Milner

## Example Programs

```sml
let
  val id = fn a => a
in
  const 1
end
```

```sml
if true then 1 else 2
```

```sml
let
  val add = fn a => fn b => a + b
in
  add 1 2
end
```

```sml
(fn a => a) 42
```

## BNF Grammar

```bnf
<EXP> ::= <INFEXP>
        | "if" <EXP> "then" <EXP> "else" <EXP>
        | "fn" <IDENT> "=>" <EXP>

<INFEXP> ::= <APPEXP>
           | <INFEXP> <OP> <INFEXP>

<APPEXP> ::= <ATEXP>
           | <APPEXP> <ATEXP>

<ATEXP> ::= <INT>
          | <BOOL>
          | <IDENT>
          | "let" <BIND> "in" <EXP> "end"
          | "(" <EXP> ")"

<BIND> ::= "val" <IDENT> "=" <EXP>

<OP> ::= "+" | "-"

<INT> ::= <DIGITS>

<DIGITS> ::= <DIGIT> <DIGITS>

<DIGIT> ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"

<BOOL> ::= "true" | "false"

<IDENT> ::= <LETTER> <LETTERS>

<LETTER> ::= "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k"
           | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v"
           | "w" | "x" | "y" | "z"

<LETTERS> ::= "" | <LETTER> <LETTERS>
```
