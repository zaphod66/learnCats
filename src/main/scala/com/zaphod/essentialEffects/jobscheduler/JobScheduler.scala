package com.zaphod.essentialEffects.jobscheduler

import cats.data.Chain
import cats.effect.{ExitCode, IO, IOApp, Outcome, Ref}
import cats.effect.kernel.Resource.ExitCase
import cats.effect.kernel.{Deferred, Fiber}
import cats.effect.std.Semaphore
import cats.implicits.{toFlatMapOps, toTraverseOps}
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
    def dequeue: (State, Option[Job.Scheduled]) =
      if (running.size >= maxRunning) this -> None
      else
        scheduled.uncons.map {
          case (h, t) => copy(scheduled = t) -> Option(h)
        }.getOrElse(this -> None)

    def addRunning(job: Job.Running): State =
      copy(running = running + (job.id -> job))

    def onComplete(job: Job.Completed): State =
      copy(running = running.removed(job.id),  completed = completed :+ job)
  }

  def scheduler(stateRef: Ref[IO, State]): JobScheduler =
    new JobScheduler {
      override def schedule(task: IO[_]): IO[Job.Id] =
        for {
          job <- Job.create(task)
          _   <- IO(job).debug
          _   <- stateRef.update(_.enqueue(job))
          _   <- stateRef.get.debug
        } yield job.id
    }

  def create(max: Int): IO[JobScheduler] =
    for {
      stateRef <- Ref[IO].of(State(max, Chain.empty, Map.empty, Chain.empty))
    } yield scheduler(stateRef)
}

trait Reactor {
  def whenAwake(
               onStart: Job.Id => IO[Unit],
               addComplete: (Job.Id, ExitCase) => IO[Unit]
               ): IO[Unit]
}

object Reactor {
  def apply(stateRef: Ref[IO, JobScheduler.State]): Reactor =
    new Reactor {
      override def whenAwake(onStart: Job.Id => IO[Unit], addComplete: (Job.Id, ExitCase) => IO[Unit]): IO[Unit] = {
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
            .flatTap(_ => addComplete(job.id, job.exitCase))

        startNextJob.iterateUntil(_.isEmpty).void
      }
    }
}

trait Zzz {
  def sleep: IO[Unit]
  def awake: IO[Unit]
}

object Zzz {
  sealed trait ZzzState
  case object Asleep extends ZzzState
  case object Awake extends ZzzState

  def apply: IO[Zzz] = for {
    state <- Ref[IO].of[ZzzState](Asleep)
    semaphore <- Semaphore[IO](0)
  } yield new Zzz {
    override def sleep: IO[Unit] = state.modify {
      case Asleep => Asleep -> IO.unit
      case Awake  => Asleep -> semaphore.acquire
    }

    override def awake: IO[Unit] = state.modify {
      case Asleep => Awake -> semaphore.release
      case Awake  => Awake -> IO.unit
    }
  }
}

// -------------

object RunScheduler extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    for {
      scheduler <- JobScheduler.create(4)
      jobId     <- scheduler.schedule(IO("whee").debug)
      _         <- IO(s"jobId: $jobId").debug
      fiber     <- IO.sleep(1000.millis).start
      _         <- IO("waiting...").debug
      _         <- fiber.join
    } yield ExitCode.Success

}
