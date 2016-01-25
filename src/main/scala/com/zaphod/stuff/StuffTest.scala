package com.zaphod.stuff

case class Person(id: Int, name: String, age: Int)

trait Repo {
  def repo: Seq[Person]
  def findById(id: Int): Option[Person]
}

trait Repo1 extends AnyRef with Repo {
  def repo = List(
    Person(1, "John Doe", 42),
    Person(2, "Eva Luator", 21),
    Person(3, "Johnny Depp", 52)
  )
}

trait Repo2 extends AnyRef with Repo {
  def repo = List(
    Person(1, "Stuart", 42),
    Person(2, "Sheldon", 21),
    Person(3, "Lennart", 52)
  )
}

trait RepoFind {
  self: Repo =>

  def findById(id: Int): Option[Person] = repo.find(p => p.id == id)
}

class RepoFindImpl1 extends AnyRef with Repo1 with RepoFind
class RepoFindImpl2 extends AnyRef with Repo2 with RepoFind

object StuffTest extends App {
  val pr1 = new RepoFindImpl1
  val pr2 = new RepoFindImpl2

  println(s"pr1.findById(2) ${pr1.findById(2)}")
  println(s"pr2.findById(2) ${pr2.findById(2)}")
}
