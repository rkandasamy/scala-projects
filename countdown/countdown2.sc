import System.nanoTime

trait Op {
  def op(a: Int, b: Int): Int
}
object Add extends Op {
  def op(a: Int, b: Int) = a + b
  override def toString = "+"
}
object Sub extends Op {
  def op(a: Int, b: Int) = a - b
  override def toString = "-"
}
object Mul extends Op {
  def op(a: Int, b: Int) = a * b
  override def toString = "*"
}
object Div extends Op {
  def op(a: Int, b: Int) = a / b
  override def toString = "/"
}

//def valid(op: Op, a: Int, b: Int): Boolean = {
//  op match {
//    case Add | Mul => true
//    case Sub => a > b
//    case Div => a % b == 0
//  }
//}
def valid(op: Op, a: Int, b: Int): Boolean = {
  op match {
    case Add => a <= b
    case Mul => a <=b && a != 1 && a != 1
    case Sub => a > b
    case Div => a % b == 0 && b != 1
  }
}

trait Expr
case class Val(n: Int) extends Expr { override def toString = n.toString }
case class App(op:Op, left: Expr, right: Expr) extends Expr {
  override def toString = "(" + left + " " + op + " " + right + ")"
}

case class Result(expr: Expr, result: Int)

def split(lst: List[Int]): List[(List[Int],List[Int])] = {
  def inner(pre: List[Int], post: List[Int], acc: List[(List[Int],List[Int])]): List[(List[Int],List[Int])] = {
    post match {
      case h :: Nil => acc
      case h :: t => inner(pre ++ List(h), t, List((pre ++ List(h), t)) ++ acc)
    }
  }
  inner(Nil, lst, List[(List[Int],List[Int])]()).reverse
}

def eval(expr: Expr): Option[Int] = {
  expr match {
    case Val(n) => if (n > 0) Some(n) else None
    case App(op, left, right) =>
      for {
        l <- eval(left)
        r <- eval(right)
        if (valid(op, l, r))
      } yield op.op(l, r)
  }
}

def choices(lst: List[Int]): List[List[Int]] = {
  def inner(level: Int, acc: List[List[Int]]): List[List[Int]] = {
    if (level == lst.length) acc
    else {
      acc ++ inner(level + 1,
        lst.flatMap(i => {
          acc.flatMap(l => if (!l.contains(i)) List(i :: l) else List.empty)
        })
      )
    }
  }
  inner(0, lst.map(i => List(i)))
}

def combine(l: Expr, r: Expr): List[Expr] = {
  List(
    App(Add, l, r),
    App(Sub, l, r),
    App(Mul, l, r),
    App(Div, l, r)
  )
}

def exprs(lst: List[Int]): List[Expr] = {
  if (lst.length == 1)
    List(Val(lst(0)))
  else {
    split(lst).flatMap(s => {
      for {
        l <- exprs(s._1)
        r <- exprs(s._2)
        op <- List(Add, Sub, Mul, Div)
      } yield App(op, l, r)
      //} yield combine(l, r)
    })
  }
}

def solutions(lst: List[Int], n: Int): List[Expr] = {
  choices(lst).flatMap(ns => {
    results(ns).filter(r => r.result == n)
  }).map(_.expr)
}

def results(lst: List[Int]): List[Result] = {
  if (lst.length == 1) {
    val n = lst(0)
    if (n > 0)
      List(Result(Val(n), n))
    else
      List.empty[Result]
  } else {
    split(lst).flatMap(s => {
      for {
        l <- results(s._1)
        r <- results(s._2)
        op <- List(Add, Sub, Mul, Div)
        if (valid(op, l.result, r.result))
      } yield {
        val app = App(op, l.expr, r.expr)
        Result(app, eval(app).getOrElse(0))
      }
    })
  }
}
//choices(List(1,3))
//println(choices(List(1,3)))
val startTime = nanoTime
solutions(List(1,3,7,10,25,50), 765).foreach(expr => println(expr))
val finishTime = System.nanoTime()
println((finishTime - startTime)/1000000)
//split(List(1,2,3,4))
