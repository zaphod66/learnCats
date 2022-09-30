package com.zaphod.essentialEffects.jobscheduler

import cats.data.Chain
import cats.effect.{ExitCode, IO, IOApp, Outcome, Ref}
import cats.effect.kernel.Resource.ExitCase
import cats.effect.kernel.{Deferred, Fiber, Resource}
import cats.effect.std.Semaphore
import cats.implicits.{toFlatMapOps, toFunctorOps, toTraverseOps}
import com.zaphod.util.Debug.DebugHelper

import java.util.UUID
import scala.concurrent.duration.DurationInt

sealed trait Job

object Job {
  case class Scheduled(id: Id, task: IO[_]) extends Job {
    def start: IO[Running] =
      for {
        exitCase <- Deferred[IO, ExitCase]
        fiber    <- task.void
          .guaranteeCase {
            case Outcome.Succeeded(_) => exitCase.complete(ExitCase.Succeeded).void
            case Outcome.Canceled()   => exitCase.complete(ExitCase.Canceled).void
            case Outcome.Errored(e)   => exitCase.complete(ExitCase.Errored(e)).void
          }
          .start
      } yield Running(id, fiber, exitCase)
  }
  case class Running(id: Id,
                     fiber: Fiber[IO, Throwable, Unit],
                     exitCase: Deferred[IO, ExitCase]
                    ) extends Job {
    val await: IO[Completed] = exitCase.get.map(Completed(id, _))
  }
  case class Completed(id: Id, exitCase: ExitCase) extends Job

  case class Id(value: UUID) extends AnyVal

  def create[A](task: IO[A]): IO[Scheduled] =
    IO(Id(UUID.randomUUID())).map(Scheduled(_, task))
}

trait JobScheduler {
  def schedule(task: IO[_]): IO[Job.Id]
}

object JobScheduler {

  case class State(
                    maxRunning: Int,
                    scheduled: Chain[Job.Scheduled] = Chain.empty,
                    running: Map[Job.Id, Job.Running] = Map.empty,
                    completed: Chain[Job.Completed] = Chain.empty
                  ) {
    def enqueue(job: Job.Scheduled): State = copy(scheduled = scheduled :+ job)
    def dequeue: (State, Option[Job.Scheduled]) = {
      if (running.size >= maxRunning) this -> None
      else
        scheduled.uncons.map {
          case (h, t) => copy(scheduled = t) -> Option(h)
        }.getOrElse(this -> None)
    }

    def addRunning(job: Job.Running): State =
      copy(maxRunning = maxRunning - 1, running = running + (job.id -> job))

    def onComplete(job: Job.Completed): State =
      copy(maxRunning = maxRunning + 1, running = running.removed(job.id),  completed = completed :+ job)
  }

  def resource(max: Int): IO[Resource[IO, JobScheduler]] =
    for {
      state <- Ref[IO].of(State(max))
      zzz   <- Zzz.apply
      scheduler = new JobScheduler {
        override def schedule(task: IO[_]): IO[Job.Id] =
          for {
            job <- Job.create(task)
            _   <- IO(job.id).debug
            _   <- state.update(_.enqueue(job))
            _   <- state.get.debug
            _   <- zzz.wakeUp
          } yield job.id
      }
      reactor = Reactor(state)
      onStart = (id: Job.Id) => IO.unit
      onComplete = (id: Job.Id, exitCase: ExitCase) => zzz.wakeUp
      loop = (zzz.sleep *> reactor.whenAwake(onStart, onComplete)).foreverM
    } yield loop.background.as(scheduler)
}

trait Reactor {
  def whenAwake(
                 onStart: Job.Id => IO[Unit],
                 onComplete: (Job.Id, ExitCase) => IO[Unit]
               ): IO[Unit]
}

object Reactor {
  def apply(stateRef: Ref[IO, JobScheduler.State]): Reactor =
    new Reactor {
      override def whenAwake(onStart: Job.Id => IO[Unit], onComplete: (Job.Id, ExitCase) => IO[Unit]): IO[Unit] = {
        def startNextJob: IO[Option[Job.Running]] =
          for {
            job     <- stateRef.modify(_.dequeue)
            running <- job.traverse(startJob)
          } yield running

        def startJob(scheduled: Job.Scheduled): IO[Job.Running] =
          for {
            running <- scheduled.start
            _       <- stateRef.update(_.addRunning(running))
            _       <- registerOnComplete(running)
            _       <- onStart(running.id).attempt
          } yield running

        def registerOnComplete(job: Job.Running) =
          job.await
            .flatMap(jobCompleted)
            .start

        def jobCompleted(job: Job.Completed): IO[Unit] =
          stateRef.update(_.onComplete(job))
            .flatTap(_ => onComplete(job.id, job.exitCase))

        startNextJob.iterateUntil(_.isEmpty).void
      }
    }
}

trait Zzz {
  def sleep: IO[Unit]
  def wakeUp: IO[Unit]
}

object Zzz {
  sealed trait ZzzState
  case class Asleep() extends ZzzState
  case class Awake() extends ZzzState

  def apply: IO[Zzz] = for {
    state <- Ref[IO].of[ZzzState](Asleep())
    semaphore <- Semaphore[IO](0)
  } yield new Zzz {
    override def sleep: IO[Unit] = state.modify {
      case Asleep() => Asleep() -> IO.unit
      case Awake()  => Asleep() -> semaphore.acquire
    }

    override def wakeUp: IO[Unit] = state.modify {
      case Asleep() => Awake() -> semaphore.release
      case Awake()  => Awake() -> IO.unit
    }
  }
}

// -------------

object RunScheduler extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    for {
      resource  <- JobScheduler.resource(2)
      _         <- resource.use { scheduler =>
        for {
          _     <- scheduler.schedule(IO.sleep(250.millis) *> IO("whee1").debug)
          _     <- scheduler.schedule(IO.sleep(240.millis) *> IO("whee2").debug)
          _     <- scheduler.schedule(IO.sleep(230.millis) *> IO("whee3").debug)
          _     <- scheduler.schedule(IO.sleep(220.millis) *> IO("whee4").debug)
          fiber <- IO.sleep(1000.millis).start
          _ <- IO("waiting...").debug
          _ <- fiber.join
        } yield ()
      }
    } yield ExitCode.Success

}
