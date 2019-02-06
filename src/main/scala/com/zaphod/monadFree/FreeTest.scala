package com.zaphod.monadFree

import com.zaphod.UnderScore.Chapter11_CRDTs.GCounterTypeClass.KeyValueStore

// https://typelevel.org/cats/datatypes/freemonad.html

import cats.free.Free

object FreeTest extends App {
  sealed trait KVStoreA[A]
  case class Put[T](key: String, value: T) extends KVStoreA[Unit]
  case class Get[T](key: String) extends KVStoreA[Option[T]]
  case class Del(key: String) extends KVStoreA[Unit]

  type KVStore[A] = Free[KVStoreA, A]

  import cats.free.Free.liftF

  def put[T](k: String, v: T): KVStore[Unit] = liftF[KVStoreA, Unit](Put[T](k, v))
  def get[T](k: String): KVStore[Option[T]]  = liftF[KVStoreA, Option[T]](Get[T](k))
  def del(k: String): KVStore[Unit]          = liftF[KVStoreA, Unit](Del(k))

  def upd[T](k: String, f: T => T): KVStore[Unit] = for {
    vM <- get[T](k)
    _  <- vM.map(v => put[T](k, f(v))).getOrElse(Free.pure(()))
  } yield ()

  def program: KVStore[Option[Int]] = for {
    _ <- put("wild-cats", 2)
    _ <- upd[Int]("wild-cats", _ + 10)
    _ <- put("tame-cats", 2)
    n <- get[Int]("wild-cats")
    _ <- del("wild-cats")
  } yield n

  import cats.{Id, ~>}

  import scala.collection.mutable

  val kvs = mutable.Map.empty[String, Any]

  def impureInterpreter: KVStoreA ~> Id = new (KVStoreA ~> Id) {
    def apply[A](fa: KVStoreA[A]): Id[A] = fa match {
      case Put(k, v) =>
        println(s"Put($k, $v)")
        kvs(k) = v
        ()
      case Get(k) =>
        println(s"Get($k)")
        kvs.get(k).map(_.asInstanceOf[A])
      case Del(k) =>
        println(s"Del($k)")
        kvs.remove(k)
        ()
    }
  }

  val result: Option[Int] = program.foldMap(impureInterpreter)

  println(s"result: ${result.fold("empty")(_.toString)}")
}
