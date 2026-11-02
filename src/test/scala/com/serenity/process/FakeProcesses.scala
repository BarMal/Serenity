package com.serenity.process

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, InputStream, OutputStream}

import cats.effect.{Deferred, IO, Ref}

/** How a fake process answers the two ways the service ends one. `Dies` exits on the request; `Ignores` stays alive. */
enum Response:
  case Dies, Ignores

/** A process or descendant the service can see. Dying clears `alive`; it never throws, as a real `destroy` does not. */
class FakeControl(
    val pid: Long,
    onDestroy: Response = Response.Dies,
    onForce: Response = Response.Dies
) extends ProcessControl:

  protected val state: Ref[IO, FakeControl.State] = Ref.unsafe[IO, FakeControl.State](FakeControl.State())

  def destroyed: IO[Boolean] = state.get.map(_.destroyRequested)
  def forced: IO[Boolean]    = state.get.map(_.forceRequested)

  protected def died: IO[Unit] = state.update(_.copy(alive = false))

  def isAlive: IO[Boolean] = state.get.map(_.alive)

  def destroy: IO[Unit] =
    state.update(_.copy(destroyRequested = true)) *> (if onDestroy == Response.Dies then died else IO.unit)

  def destroyForcibly: IO[Unit] =
    state.update(_.copy(forceRequested = true)) *> (if onForce == Response.Dies then died else IO.unit)

object FakeControl:
  final case class State(alive: Boolean = true, destroyRequested: Boolean = false, forceRequested: Boolean = false)

/** A started process. Its descendants are visible only while it is alive: once it dies they are reparented away from
  * it, as on a real system, so the service has to take its snapshot before it touches the root.
  */
final class FakeProcess(
    pid: Long,
    val children: List[FakeControl] = Nil,
    onDestroy: Response = Response.Dies,
    onForce: Response = Response.Dies,
    output: IO[Array[Byte]] = IO.pure(Array.emptyByteArray),
    exit: Int = 0
) extends FakeControl(pid, onDestroy, onForce)
    with RunningProcess:

  private val exited = Deferred.unsafe[IO, Int]

  override protected def died: IO[Unit] = super.died *> exited.complete(exit).void

  /** The process ended by itself, as a command that simply finishes does. */
  def finish: IO[Unit] = died

  def descendants: IO[List[ProcessControl]] = isAlive.map(alive => if alive then children else Nil)

  def exitCode: IO[Int] = exited.get

  def readStdout: IO[Array[Byte]] = output

  def discardStderr: IO[Unit] = IO.unit

  val stdin: OutputStream = new ByteArrayOutputStream()
  val stdout: InputStream = new ByteArrayInputStream(Array.emptyByteArray)
  val stderr: InputStream = new ByteArrayInputStream(Array.emptyByteArray)

/** Hands out the processes it was given, in order, and remembers what it was asked to start. */
final class FakeLauncher(processes: List[FakeProcess], failToStart: Option[Throwable] = None) extends ProcessLauncher:

  private val queue   = Ref.unsafe[IO, List[FakeProcess]](processes)
  private val started = Ref.unsafe[IO, Vector[ProcessSpec]](Vector.empty)

  def specs: IO[Vector[ProcessSpec]] = started.get

  def start(spec: ProcessSpec): IO[RunningProcess] =
    failToStart match
      case Some(error) => IO.raiseError(error)
      case None =>
        started.update(_ :+ spec) *> queue.modify {
          case next :: rest => (rest, IO.pure(next: RunningProcess))
          case Nil => (Nil, IO.raiseError(new IllegalStateException("the fake launcher has no more processes")))
        }.flatten
