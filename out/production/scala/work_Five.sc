def max(a: Int, b: Int) =
  if (a > b)
    a
  else
    b
def evaluate(a: Int, b: Int, c: Int, f: (Int, Int) => Int) =
  f(f(a, b), c)
evaluate(1, 2, 3, max) == 3
evaluate(2, 1, 3, max) == 3
evaluate(3, 1, 2, max) == 3