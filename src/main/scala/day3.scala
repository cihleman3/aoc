val regex = """mul\((\d+) *?, *?(\d+)\)""".r

def parse(text: String): Iterator[(Int, Int)] = regex.findAllMatchIn(text) map :
  case regex(m, n) => m.toInt -> n.toInt

def foo(text: String) =
  parse(text).map(_ * _).sum

@main
def day3() =
  val input = """xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"""
  println(foo(input))
