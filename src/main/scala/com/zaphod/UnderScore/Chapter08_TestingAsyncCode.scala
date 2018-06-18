package com.zaphod.UnderScore

object Chapter08_TestingAsyncCode extends App {

  import scala.concurrent.Future

  import scala.language.higherKinds

  object Exercise {
    import cats.instances.list._
    import cats.syntax.traverse._
    import cats.syntax.functor._
    import cats.Id
    import cats.Applicative

    trait UptimeClient[F[_]] {
      def getUptime(hostname: String): F[Int]
    }

    class UptimeService[F[_]: Applicative](client: UptimeClient[F]) {
      def getTotalUptime(hostnames: List[String]): F[Int] = hostnames.traverse(client.getUptime).map(_.sum)
    }

    trait RealUptimeClient extends UptimeClient[Future] {
      override def getUptime(hostname: String): Future[Int]
    }

    trait TestUptimeClient extends UptimeClient[Id] {
      override def getUptime(hostname: String): Int
    }

    class TestUpdateClientImpl(hosts: Map[String, Int]) extends TestUptimeClient {
      override def getUptime(hostname: String): Int = hosts.getOrElse(hostname, 0)
    }

    def testTotalUptime = {
      val hosts   = Map("host1" -> 10, "host3" -> 6)
      val client  = new TestUpdateClientImpl(hosts)
      val service = new UptimeService(client)
      val actual  = service.getTotalUptime(hosts.keys.toList)
      val expect  = hosts.values.sum

      println(actual == expect)
    }
  }

  Exercise.testTotalUptime
}
