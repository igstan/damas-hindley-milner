let
  val arith = fn a => fn b => fn c => a + b + c
in
  arith 1 2 (if true then 3 else 4)
end
