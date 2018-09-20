def product(f: Int => Int)(a: Int, b: Int): Int =
  if (a > b) 1 else f(a) * product(f)(a + 1, b)

def factorial(n: Int): Int = product(x => x)(1, n)

factorial(6)

def mapReduce(f: Int => Int, combine: (Int, Int) => Int, unit: Int)
             (a: Int, b: Int): Int = {
  if (a > b) unit
  else combine(f(a), mapReduce(f, combine, unit)(a+1, b))
}

def product2(f: Int => Int)(a: Int, b: Int): Int =
  mapReduce(f, (x, y) => x * y, 1)(a, b)

product2(x => x*x)(3,4)

def sum(f: Int => Int)(a: Int, b: Int): Int =
  mapReduce(f, (x, y) => x + y, 0)(a, b)

sum(x => x)(0, 5)

//def sum(f: Int => Int)(a: Int, b: Int): Int = {
//  def loop(a: Int, acc: Int): Int = {
//    if (a > b) acc
//    else loop(a+1, acc + f(a))
//  }
//  loop(a, 0)
//}
//
//sum(x => x)(1,5)

