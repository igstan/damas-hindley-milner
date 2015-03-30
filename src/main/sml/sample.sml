let
  val addThree = fn a => fn b => fn c => a + b + c
in
  addThree 1 2 (if true then 3 else 4)
end
