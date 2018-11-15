package com.zaphod.UnderScore

import scala.language.higherKinds

object Chapter04_Monad extends App {

  object Definition {

    trait Monad[F[_]] {
      def pure[A](a: => A): F[A]

      def flapMap[A, B](fa: F[A])(f: A => F[B]): F[B]

      def map[A, B](fa: F[A])(f: A => B): F[B] = flapMap(fa)(a => pure(f(a)))
    }

  }

  object Instances {

    import cats.Monad

    import cats.instances.option._

    val opt1 = Monad[Option].pure(3)
    val opt2 = Monad[Option].flatMap(opt1)(a => Some(a + 2))
    val opt3 = Monad[Option].map(opt2)(a => 100 * a)

    import cats.instances.list._

    val lis1 = Monad[List].pure(3)
    val lis2 = Monad[List].flatMap(List(1, 2, 3))(a => List(a, a * 10))
    val lis3 = Monad[List].map(lis2)(a => a + 123)

    import cats.instances.vector._

    val vec1 = Monad[Vector].flatMap(Vector(1, 2, 3))(a => Vector(a, a * 10))

    import cats.instances.future._
    import scala.concurrent._
    import scala.concurrent.duration._
    import scala.concurrent.ExecutionContext.Implicits.global

    val mf = Monad[Future]
    val fut1 = mf.flatMap(mf.pure(1))(x => mf.pure(x + 2))
    val res1 = Await.result(fut1, 1.second)
  }

  object Syntax {

    import cats.syntax.flatMap._
    import cats.syntax.functor._
    import cats.syntax.applicative._

    import cats.instances.option._
    import cats.instances.list._

    val opt1 = 1.pure[Option]
    val lis1 = 1.pure[List]

    import cats.Monad

    def sumSquare[F[_] : Monad](a: F[Int], b: F[Int]): F[Int] = a.flatMap(x => b.map(y => x * x + y * y))

    val sum1 = sumSquare(3.pure[Option], 4.pure[Option])
    val sum2 = sumSquare(List(1, 2, 3), List(4, 5))

    println(s"sum1: $sum1")
    println(s"sum2: $sum2")
  }

  object IdentityMonad {

    import cats.Monad
    import Chapter04_Monad.Syntax.sumSquare

    type Id[A] = A

    implicit val idMonad = new Monad[Id] {
      override def pure[A](x: A): Id[A] = x

      override def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = f(fa)

      override def map[A, B](fa: Id[A])(f: A => B): Id[B] = f(fa)

      override def tailRecM[A, B](a: A)(f: A => Id[Either[A, B]]): Id[B] = ???
    }

    val sum1 = sumSquare(2: Id[Int], 3: Id[Int])

    println(s"sum1: $sum1")
  }

  object EitherUse {

    import Chapter04_Monad.Syntax.sumSquare
    import cats.instances.either._

    import cats.syntax.either._

    val a = 3.asRight[String]
    val b = 4.asRight[String]

    val c = sumSquare(a, b)

    val d = Either.catchOnly[NumberFormatException]("foo".toInt)
    val e = Either.catchNonFatal(sys.error("Boom"))

    val f = Either.fromOption(Option(1), "Boom")
    val g = Either.fromOption(None, "Boom")

    val h = 0.asRight[String].ensure("Must be positive")(_ > 0)

    val i = "error".asLeft[Int].recover { case _: String => -1 }
    val j = "error".asLeft[Int].recoverWith { case _: String => 1.asRight[String] }

    val k = "foo".asLeft[Int].leftMap(_.reverse)
    val l = 6.asRight[String].bimap(_.reverse, _ * 7)
    val m = "error".asLeft[Int].bimap(_.reverse, _ * 7)

    //    println(s"a: $a")
    //    println(s"b: $b")
    //    println(s"c: $c")
    println(s"d: $d")
    println(s"e: $e")
    //    println(s"f: $f")
    //    println(s"g: $g")
    //    println(s"h: $h")
  }

  object MonadErrorUse {
    import cats.MonadError
    import cats.instances.either._

    type ErrorOr[A] = Either[String, A]
    val  monadError = MonadError[ErrorOr, String]

    val success1 = monadError.pure(42)
    val failure1 = monadError.raiseError("Boom")

    monadError.handleError(failure1) {
      case "Boom" => monadError.pure("It's ok")
      case _      => monadError.raiseError("It's not ok")
    }

//    import cats.syntax.either._

    monadError.ensure(success1)("Number too low!")(_ > 1000)

    import cats.syntax.applicative._
    import cats.syntax.applicativeError._
    import cats.syntax.monadError._

    val success2 = 42.pure[ErrorOr]
    val failure2 = "Boom".raiseError[ErrorOr, Int]

    val res = success2.ensure("Number too low!")(_ > 1000)
  }

  object EvalMonad {
//    val x = { println(s"Computing x!"); math.random }         // eager, now
//    lazy val y =  { println(s"Computing y!"); math.random }   // lazy, later
//    def z = { println(s"Computing z!"); math.random } // lazy, always
//
//    println("------")
//    println(s"x: $x")
//    println(s"x: $x")
//
//    println(s"y: $y")
//    println(s"y: $y")
//
//    println(s"z: $z")
//    println(s"z: $z")

    import cats.Eval

//    val n = Eval.now    { println(s"Computing n!"); math.random + 1000 }
//    val l = Eval.later  { println(s"Computing l!"); math.random + 1000 }
//    val a = Eval.always { println(s"Computing a!"); math.random + 1000 }
//
//    println("------")
//    println(s"n: ${n.value}")
//    println(s"n: ${n.value}")
//
//    println(s"l: ${l.value}")
//    println(s"l: ${l.value}")
//
//    println(s"a: ${a.value}")
//    println(s"a: ${a.value}")
//
//    val greeting = Eval.
//      always { println("Step1"); "Hello" }.
//      map { s => println("Step2"); s"$s world" }
//
//    println(s"greeting: ${greeting.value}")
//    println(s"greeting: ${greeting.value}")

    val result = for {
      x <- Eval.now    { println("Calculating x"); 30 }
      y <- Eval.later  { println("Calculating y"); 10 }
      z <- Eval.always { println("Calculating z");  2 }
    } yield {
      println("Adding x, y and z")
      x + y + z
    }

//    println(s"result: ${result.value}")
//    println(s"result: ${result.value}")

    val saying = Eval.
      always { println("Step 1"); "The cat" }.
      map    { s => println("Step 2"); s"$s sat on" }.
      memoize.
      map    { s => println("Step 3"); s"$s the mat" }

    println(s"saying: ${saying.value}")
    println(s"saying: ${saying.value}")

    // Trampolining and Eval.defer

    def fac1(n: BigInt): BigInt = if (n == 1) 1 else n * fac1(n - 1)  // Stackoverflow

    def fac2(n: BigInt): Eval[BigInt] = if (n == 1) Eval.now(1) else Eval.defer(fac2(n - 1).map(_ * n))

    def fac3(n: BigInt): BigInt = {
      def go(n: BigInt): Eval[BigInt] = if (n == 1) Eval.now(1) else Eval.defer(go(n - 1).map(_ * n))

      go(n).value
    }

    // val f1 = fac1(5000)
    val f2 = fac2(5000)
    val f3 = fac3(5000)

//    println(s"f2: ${f2.value}")

    def foldRight[A, B](as: List[A], acc: B)(f: (A, B) => B): B = {
      def go(as: List[A], acc: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = as match {
        case h :: t => Eval.defer(f(h, go(t, acc)(f)))
        case Nil    => acc
      }

      go(as, Eval.now(acc)) { (a, b) => b.map( f(a, _) ) }.value
    }

    val l = List.fill(50000)(1)
    val s = foldRight(l, 0)(_ + _)

    println(s"s = $s")
  }

  object WriterMonad {
    import cats.data.Writer
    import cats.instances.vector._    // for Monoid
    import cats.syntax.applicative._  // for pure
    import cats.syntax.writer._

    type Logged[A] = Writer[Vector[String], A]

    val w1 = 42.pure[Logged]
    val w2 = Vector("msg1", "msg2", "msg3").tell
    val w3 = Writer(Vector("msg1", "msg2", "msg3"), 42)
    val w4 = 42.writer(Vector("msg1", "msg2", "msg3"))

    val r4 = w4.value
    val l4 = w4.written
    val (r, l) = w4.run

//    println(s"w4: $w4")
//    println(s"r4: $r4")
//    println(s"l4: $l4")

    val w5 = for {
      a <- 10.pure[Logged]
      _ <- Vector("a, b, c").tell
      b <- 32.writer(Vector("x", "y", "z"))
    } yield a + b

    println(s"w5: $w5")

    val w6 = w5.mapWritten(_.map(_.toUpperCase))

//    println(s"w6: $w6")

    val w7 = w5.bimap(_.map(_ + "!"), _ * 100)
    val w8 = w5.mapBoth { (l1, r1) =>
      val l2 = l1.map(_ + "!")
      val r2 = r1 * 100

      (l2, r2)
    }

    println(s"w7: $w7")
//    println(s"w8: $w8")

    def slowly[A](body: => A) = try body finally Thread.sleep(100)

    def fac1(n: Int): Int = {
      val ans = slowly { if (n == 1) 1 else n * fac1(n - 1) }
      println(f"fac1($n) = $ans%4d Thread: ${ Thread.currentThread.getId }")
      ans
    }

    def fac2(n: Int): Logged[Int] = {
      for {
        ans <- if (n == 0) {
                 1.pure[Logged]
               } else {
                 slowly { fac2(n - 1). map(_ * n) }
               }
        _ <- Vector(s"fac2($n) = $ans").tell
      } yield ans
    }
  }

  object ReaderMonad {
    import cats.data.Reader

    case class Cat(name: String, favFood: String)

    val catName:   Reader[Cat, String] = Reader(cat => cat.name)
    val greetCat:  Reader[Cat, String] = catName.map(name => s"Hello $name")
    val feedCat:   Reader[Cat, String] = Reader(cat => s"Have a nice bowl of ${cat.favFood}")
    val greetFeed = for {
      greet <- greetCat
      feed  <- feedCat
    } yield s"$greet. $feed."

    println(s"""${greetFeed(Cat("Garfield", "lasagne"))}""")
    println(s"""${greetFeed(Cat("Heathcliff", "junk food"))}""")

    case class DB(
      userNames: Map[Int, String],
      passwords: Map[String, String]
    )

    type DBReader[A] = Reader[DB, A]

    def findUser(userId: Int): DBReader[Option[String]] = Reader { db => db.userNames.get(userId) }
    def checkPass(userName: String, password: String): DBReader[Boolean] = Reader { db => db.passwords.get(userName).contains(password) }

    import cats.syntax.applicative._

    def checkLogin(userId: Int, password: String): DBReader[Boolean] = for {
      name <- findUser(userId)
      pass <- name.fold(false.pure[DBReader])(checkPass(_, password))
    } yield pass

    val users = Map( 1 -> "Dave", 2 -> "Kate", 3 -> "Eric")
    val pswds = Map( "Dave" -> "evaD", "Kate" -> "etaK", "Eric" -> "cirE")
    val db    = DB(users, pswds)

    println(s"""checkLogin(1, "evaD") = ${ checkLogin(1, "evaD").run(db) }""")
    println(s"""checkLogin(4, "evaD") = ${ checkLogin(4, "evaD").run(db) }""")
  }

  object StateMonad {
    import cats.data.State

    val a = State[Int, String] { state => (state, s"The state is $state") }
    val (r1, s1) = a.run(10).value
    val s2 = a.runS(10).value // return only the state
    val r2 = a.runA(10).value // return only the result

    println(s"r1 = $r1, s1 = $s1")

    val step1 = State[Int, String] { num =>
      val sta = num + 1
      val res = s"Result of step1 is $sta"

      (sta, res)
    }

    val step2 = State[Int, String] { num =>
      val sta = num * 2
      val res = s"Result of step2 is $sta"

      (sta, res)
    }

    val both = for {
      a <- step1
      b <- step2
    } yield (a, b)

    val (s3, r3) = both.run(20).value

    println(s"r3 = $r3, s3 = $s3")

    type CalcState[A] = State[List[Int], A]

    def operand(num: Int): CalcState[Int] = State[List[Int], Int] { stack => (num :: stack, num) }
    def operator(f: (Int, Int) => Int): CalcState[Int] = State[List[Int], Int] {
      case x :: y :: tail =>
        val res = f(x, y)
        (res :: tail, res)

      case _ => sys.error("fail!")
    }

    def evalSym(sym: String): CalcState[Int] = {
      sym match {
        case "+" => operator(_ + _)
        case "-" => operator(_ - _)
        case "*" => operator(_ * _)
        case "/" => operator(_ / _)
        case num => operand(num.toInt)
      }
    }

    val e1 = evalSym("2").runA(List.empty[Int]).value
    val e2 = evalSym("+").runA(List(2, 3)).value
    val e3 = (for {
      _ <- evalSym("3")
      _ <- evalSym("1")
      r <- evalSym("+")
    } yield r).runA(List.empty[Int]).value

    println(s"e1 = $e1, e2 = $e2, e3 = $e3")

    def evalAll(in: List[String]): CalcState[Int] = {
      import cats.syntax.applicative._

      in.foldLeft(0.pure[CalcState]) { (acc, x) => acc.flatMap( _ => evalSym(x)) }
    }

    val e4 = evalAll(List("2", "3", "+", "4", "*")).runA(List.empty[Int]).value

    def evalInput(input: String): Int = evalAll(input.split(" ").toList).runA(List.empty[Int]).value

    val e5 = evalInput("1 2 + 3 4 + * 2 *")

    val prog = for {
      - <- evalAll(List("1", "2", "3", "*", "+"))
      _ <- evalAll(List("2", "3", "+"))
      r <- evalSym("-")
    } yield r

    val e6 = prog.runA(List.empty[Int]).value

    println(s"e4 = $e4, e5 = $e5, e6 = $e6")
  }

  object TreeMonad {
    import cats.Monad
    import cats.syntax.applicative._

    sealed trait Tree[+A]
    final case class Branch[A](l: Tree[A], r: Tree[A]) extends Tree[A]
    final case class Leaf[A](value: A) extends Tree[A]

    def branch[A](l: Tree[A], r: Tree[A]): Tree[A] = Branch(l ,r)
    def leaf[A](value: A): Tree[A] = Leaf(value)

    implicit val treeMonad = new Monad[Tree] {
      override def pure[A](a: A): Tree[A] = leaf(a)

      override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = fa match {
        case Leaf(a)      => f(a)
        case Branch(l, r) => branch(flatMap(l)(f), flatMap(r)(f))
      }

      // not tail-recursive
      override def tailRecM[A, B](value: A)(f: A => Tree[Either[A, B]]): Tree[B] = f(value) match {
        case Leaf(Right(a)) => leaf(a)
        case Leaf(Left(a))  => tailRecM(a)(f)
        case Branch(l, r)   => branch(
          flatMap(l) {
            case Left(a)  => tailRecM(a)(f)
            case Right(a) => pure(a)
          },
          flatMap(r) {
            case Left(a)  => tailRecM(a)(f)
            case Right(a) => pure(a)
          }
        )
      }
    }

    import cats.syntax.functor._
    import cats.syntax.flatMap._

    val t0 = branch(branch(leaf("a"), branch(leaf("b"), leaf("c"))), leaf("d"))
    val t1 = branch(leaf(100), leaf(200))
    val t2 = t1.flatMap(a => branch(leaf(a - 1), leaf(a + 1)))
    val t3 = for {
      a <- t1
      b <- branch(leaf(a - 10), leaf(a + 10))
      c <- branch(leaf(b - 1), leaf(b + 1))
    } yield c

    def blowUp(t: Tree[Int]): Tree[Int] = for {
      a <- t
      b <- branch(leaf(a - 100), leaf(a + 100))
      c <- branch(leaf(b -  10), leaf(b +  10))
      d <- branch(leaf(c -   1), leaf(c +   1))
    } yield d

    def f1(t: Tree[Int]): Tree[Int] = for {
      a <- t
      b <- branch(leaf(a - 1), leaf(a + 1))
    } yield b

    println(s"t0: $t0")
    println(s"t1: $t1")
    println(s"t2: $t2")
    println(s"t3: $t3")

    trait Incrementable[A] {
      def zero: A
      def succ(a: A): A
    }

    implicit val intIncrementable = new Incrementable[Int] {
      override def zero: Int = 0
      override def succ(a: Int): Int = a + 1
    }

    val charIncrementable = new Incrementable[Char] {
      override def zero: Char = 'a'
      override def succ(a: Char): Char = (a.toInt + 1).toChar
    }

    implicit class Incr[A : Incrementable](a: A) {
      private val incr = implicitly[Incrementable[A]]
      def succ: A = incr.succ(a)
    }

    def labelTree[A, B](tree: Tree[A])(implicit inc: Incrementable[B]): Tree[(A, B)] = {
      import cats.data.State

      def go(tree: Tree[A]): State[B, Tree[(A, B)]] = State[B, Tree[(A, B)]] { s1 =>
        tree match {
          case Leaf(i)      => (s1.succ, leaf(i, s1))
          case Branch(l, r) =>
            val (s2, ll) = go(l).run(s1).value
            val (s3, rl) = go(r).run(s2).value

            (s3, branch(ll, rl))
        }
      }

      go(tree).runA(inc.zero).value
    }

    val t4 = labelTree(t1)
    val t5 = labelTree(t2)
    val t6 = labelTree(t3)
    val t7 = labelTree(t3)(charIncrementable)

    println(s"t4: $t4")
    println(s"t5: $t5")
    println(s"t6: $t6")
    println(s"t7: $t7")

    val t8 = labelTree(t0)
    val t9 = labelTree(t0)(charIncrementable)

    println(s"t8: $t8")
    println(s"t9: $t9")
  }

//  Syntax
//  IdentityMonad
//  EitherUse

//  MonadEvalUse

  // testing WriterMonad
//  import scala.concurrent._
//  import scala.concurrent.ExecutionContext.Implicits.global
//  import scala.concurrent.duration._
//
//  import WriterMonad._
//
//  val v1 = Vector(Future(fac1(3)), Future(fac1(5)), Future(fac1(4)))
//  val r1 = Await.result(Future.sequence(v1), 5.seconds)
//
//  val v2 = Vector(Future(fac2(3)), Future(fac2(5)), Future(fac2(4)))
//  val r2 = Await.result(Future.sequence(v2), 5.seconds)
//  r2.map(_.run) foreach { case (log, res) =>
//    println(f"""$res%4d: ${ log.mkString(", ") }""")
//  }

//  ReaderMonad
  StateMonad
  TreeMonad
}
