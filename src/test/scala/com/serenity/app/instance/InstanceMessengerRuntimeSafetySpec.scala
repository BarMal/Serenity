package com.serenity.app.instance

import java.net.{StandardProtocolFamily, UnixDomainSocketAddress}
import java.nio.channels.ServerSocketChannel
import java.nio.file.Path
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}
import java.util.concurrent.{Executors, LinkedBlockingQueue}

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.DurationInt

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import com.serenity.TestTemp
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{Logger, LoggerFactory, LoggerName}

/** Cancelling a blocked NIO channel call leaves `ClosedByInterruptException`'s interrupt flag on the thread, and
  * `IO.interruptible` does not clear it. On the global runtime that thread is a converted compute worker, which Cats
  * Effect recycles flag and all; the worker then silently shuts the whole pool down the next time it parks: the CI runs
  * that went unresponsive.
  *
  * Which thread gets recycled is a race, so this checks the cause instead: the blocking calls run on a single thread of
  * this spec's own, which records every task that hands it back still interrupted.
  */
class InstanceMessengerRuntimeSafetySpec extends AnyFlatSpec with Matchers:

  private given LoggerFactory[IO] = Slf4jFactory.create[IO]
  private val logger: Logger[IO]  = LoggerFactory[IO].getLogger(using LoggerName("InstanceMessengerRuntimeSafetySpec"))

  private def socketPath(): Path = TestTemp.directory("sock").resolve("i.sock")

  /** One thread running blocking tasks in turn, counting those that leave its interrupt flag set. */
  final private class FlagCheckingBlocker extends ExecutionContext:
    val leftInterrupted: AtomicInteger = AtomicInteger(0)
    private val tasks                  = LinkedBlockingQueue[Runnable]()
    private val stopped                = AtomicBoolean(false)
    private val thread                 = Thread(() => loop(), "messenger-safety-blocker")
    thread.setDaemon(true)
    thread.start()

    def execute(task: Runnable): Unit         = tasks.put(task)
    def reportFailure(cause: Throwable): Unit = cause.printStackTrace()

    def stop(): Unit =
      stopped.set(true)
      thread.interrupt()

    @tailrec
    private def loop(): Unit =
      if !stopped.get then
        try tasks.take().run()
        catch case _: InterruptedException => if !stopped.get then leftInterrupted.incrementAndGet(): Unit
        loop()

  private def withFlagCheckingRuntime[A](body: (IORuntime, FlagCheckingBlocker) => A): A =
    val compute = Executors.newFixedThreadPool(2)
    val blocker = FlagCheckingBlocker()
    val runtime = IORuntime
      .builder()
      .setCompute(ExecutionContext.fromExecutor(compute), () => compute.shutdown())
      .setBlocking(blocker, () => blocker.stop())
      .build()
    try body(runtime, blocker)
    finally runtime.shutdown()

  "Cancelling the instance listener while it waits for a connection" should "leave no interrupt flag on its thread" in
    withFlagCheckingRuntime { (runtime, blocker) =>
      val socket = socketPath()

      val cancelWhileAccepting = InstanceMessenger.serve(socket, logger).use(_ => IO.sleep(100.millis))
      val afterwards           = IO.blocking(()).replicateA_(3)

      (cancelWhileAccepting >> afterwards).unsafeRunTimed(10.seconds)(using runtime) shouldBe Some(())
      blocker.leftInterrupted.get shouldBe 0
    }

  "Cancelling a forward to an instance that never answers" should "leave no interrupt flag on its thread" in
    withFlagCheckingRuntime { (runtime, blocker) =>
      val socket = socketPath()

      val silentListener = IO.blocking {
        val server = ServerSocketChannel.open(StandardProtocolFamily.UNIX)
        server.bind(UnixDomainSocketAddress.of(socket))
        server
      }
      val forwardCancelledMidExchange =
        silentListener.flatMap(server => InstanceMessenger.forward(socket, Nil).guarantee(IO.blocking(server.close())))
      val afterwards = IO.blocking(()).replicateA_(3)

      (forwardCancelledMidExchange >> afterwards).unsafeRunTimed(10.seconds)(using runtime) shouldBe Some(())
      blocker.leftInterrupted.get shouldBe 0
    }
