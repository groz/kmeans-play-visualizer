abstract class Maybe[+T] {
  def map[U](f: T => U): Maybe[U]
}
case class Just[T](get: T) extends Maybe[T] {
  def map[U](f: T => U) = Just(f(get))
}

case object Not extends Maybe[Nothing] {
  def map[U](f: Nothing => U) = Not
}

val x: Maybe[Int] = Just(100)

def isEven(x: Maybe[Int]): Maybe[Boolean] = x match {
  case Just(value) => Just(value % 2 == 0)
  case Not => Not
}

def half(x: Maybe[Int]): Maybe[Double] = x match {
  case Just(value) => Just(value / 2.0)
  case Not => Not
}

case class Person(name: String, height: Maybe[Int], weight: Maybe[Int]) {
  def isOverweight = height match {
    case Just(h) => weight match {
      case Just(w) => Just(w + 120 < h)
      case Not => Not
    }
    case Not => Not
  }
}

case class Person(name: String, nickname: Maybe[String], height: Maybe[Int], weight: Maybe[Int])

def fullNameLength(p: Person): Maybe[Int] = p.nickname match {
  case Just(nickname) => Just(p.name.length + nickname.length)
  case Not => Not
}

def calcBMI(p: Person) = p.weight.toDouble / (p.height * p.height) // division by zero exception


/*
abstract class Maybe[+T] {
  def map[U](f: T => U): Maybe[U]
}
case class Just[T](get: T) extends Maybe[T] {
  def map[U](f: T => U) = Just(f(get))
}

case object Not extends Maybe[Nothing] {
  def map[U](f: Nothing => U) = Not
}

val x: Maybe[Int] = Just(100)

def isEven(x: Maybe[Int]): Maybe[Boolean] = x.map(_ % 2 == 0)

def half(x: Maybe[Int]): Maybe[Double] = x.map(_ / 2.0)

case class Person(name: String, height: Maybe[Int], weight: Maybe[Int]) {
  def isOverweight = height.map { h =>
    weight.map(w => w + 120 < h)
  }
}


 */