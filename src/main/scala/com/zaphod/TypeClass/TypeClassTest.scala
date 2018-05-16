package com.zaphod.TypeClass

sealed trait Json
final case class JsObject(get: Map[String, Json]) extends Json
final case class JsString(get: String) extends Json
final case class JsNumber(get: Double) extends Json
case object JsNull extends Json

trait JsonWriter[A] {
  def write(value: A): Json
}

final case class Person(name: String, email: String)

object JsonWriterInstances {
  implicit val stringWriter: JsonWriter[String] = new JsonWriter[String] {
    override def write(value: String): Json = JsString(value)
  }

  implicit val personWriter: JsonWriter[Person] = new JsonWriter[Person] {
    override def write(value: Person): Json = JsObject( Map(
      "name" -> JsString(value.name),
      "email" -> JsString(value.email)
    ))
  }

  implicit def optionWriter[A](implicit writer: JsonWriter[A]): JsonWriter[Option[A]] = new JsonWriter[Option[A]] {
    override def write(option: Option[A]): Json = option match {
      case Some(a) => writer.write(a)
      case None    => JsNull
    }
  }
}

object Json {
  def toJson[A](value: A)(implicit w: JsonWriter[A]): Json = w.write(value)
}

object JsonSyntax {
  implicit class JsonWriterOps[A](value: A) {
    def toJson(implicit w: JsonWriter[A]): Json = w.write(value)
  }
}

object TypeClassTest extends App {
  import JsonWriterInstances._

  val json1 = Json.toJson(Person("Mona", "mona@email.com"))

  import JsonSyntax._

  val json2 = Person("Mona", "mona@email.com").toJson

  val json3 = Json.toJson(Option(Person("Mona", "mona@email.com")))
  val json4 = Json.toJson(Option(Person("Mona", "mona@email.com")))(optionWriter(personWriter))

  println(s"json1: $json1")
  println(s"json2: $json2")
  println(s"json3: $json3")
  println(s"json4: $json4")
}
