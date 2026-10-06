package com.serenity.app

import java.nio.file.{Path, Paths}

import scala.concurrent.duration.DurationInt

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref, Resource}
import com.serenity.app.instance.{Delivery, LaunchRole}
import com.serenity.state.manager.QuitOutcome
import com.serenity.testkit.AwaitCondition
import fs2.Stream
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DesktopHooksSpec extends AnyFlatSpec with Matchers:

  private val launched = Paths.get("/work/launched.md")
  private val dropped  = Paths.get("/work/dropped.md")
  private val another  = Paths.get("/work/another.md")

  /** Stands in for the AWT `Desktop`: records what was installed and lets a spec raise the events itself. */
  final private class FakeDesktop(supportsOpen: Boolean = true, supportsQuit: Boolean = true) extends DesktopEvents:
    private val target: Ref[IO, Option[DesktopTarget]] = Ref.unsafe(None)
    private val attachments: Ref[IO, Int]              = Ref.unsafe(0)

    def attach(newTarget: DesktopTarget): Resource[IO, Unit] =
      Resource.make(target.set(Some(newTarget)) >> attachments.update(_ + 1))(_ => target.set(None))

    def raiseOpen(paths: List[Path]): IO[Unit] =
      target.get.map(current => if supportsOpen then current.foreach(_.onOpen(paths)))
    def raiseQuit(response: QuitResponse): IO[Unit] =
      target.get.map(current => if supportsQuit then current.foreach(_.onQuit(response)))
    def attached: IO[Int]      = attachments.get
    def listening: IO[Boolean] = target.get.map(_.isDefined)

  private def answersAndResponse: IO[(Ref[IO, List[String]], QuitResponse)] =
    Ref.of[IO, List[String]](Nil).map { answers =>
      (answers, QuitResponse(answers.update(_ :+ "perform"), answers.update(_ :+ "cancel")))
    }

  "DesktopHooks.install" should "never touch the desktop in terminal mode" in {
    val desktop  = FakeDesktop()
    val attached = DesktopHooks.install(desktop, gui = false).use(_ => desktop.attached)
    attached.unsafeRunSync() shouldBe 0
  }

  it should "attach to the desktop for the launch in windowed mode, and detach when it ends" in {
    val desktop = FakeDesktop()
    val during  = DesktopHooks.install(desktop, gui = true).use(_ => desktop.listening)
    during.unsafeRunSync() shouldBe true
    desktop.attached.unsafeRunSync() shouldBe 1
    desktop.listening.unsafeRunSync() shouldBe false
  }

  it should "carry on without hooks when the desktop supports neither" in {
    val desktop = FakeDesktop(supportsOpen = false, supportsQuit = false)
    val opened = DesktopHooks
      .install(desktop, gui = true)
      .use(hooks => desktop.raiseOpen(List(dropped)) >> hooks.openedFiles.interruptAfter(100.millis).compile.toList)
    opened.unsafeRunSync() shouldBe Nil
  }

  "A file opened through the desktop" should "reach the primary's open path alongside forwarded launches" in {
    val program = DesktopHooks.install(FakeDesktop(), gui = true).use { hooks =>
      hooks.opensFor(LaunchRole.Primary(Stream.emit(List(launched)))).take(1).compile.toList
    }
    program.unsafeRunSync() shouldBe List(List(launched))
  }

  it should "reach the primary when it arrives after a forwarded launch" in {
    val desktop = FakeDesktop()
    val program = DesktopHooks.install(desktop, gui = true).use { hooks =>
      val role = LaunchRole.Primary(Stream.emit(List(launched)))
      hooks.opensFor(role).take(2).compile.toList.both(desktop.raiseOpen(List(dropped, another))).map(_._1)
    }
    program.unsafeRunSync().toSet shouldBe Set(List(launched), List(dropped, another))
  }

  it should "be the only source of opens in an isolated launch" in {
    val desktop = FakeDesktop()
    val program = DesktopHooks.install(desktop, gui = true).use { hooks =>
      desktop.raiseOpen(List(dropped)) >>
        hooks.opensFor(LaunchRole.Isolated(Paths.get("/tmp/iso"))).take(1).compile.toList
    }
    program.unsafeRunSync() shouldBe List(List(dropped))
  }

  it should "be forwarded to the running instance by a launch that stepped aside" in {
    val desktop = FakeDesktop()
    val program = for
      forwarded <- Ref.of[IO, List[List[Path]]](Nil)
      _ <- DesktopHooks.install(desktop, gui = true).use { hooks =>
        desktop.raiseOpen(List(dropped, another)) >>
          DesktopHooks.forwardQueued(hooks, paths => forwarded.update(_ :+ paths).as(Delivery.Delivered), 200.millis)
      }
      seen <- AwaitCondition.awaitValue(forwarded.get)(_.nonEmpty)
    yield seen
    program.unsafeRunSync() shouldBe List(List(dropped, another))
  }

  it should "not be forwarded when nothing was queued" in {
    val program = for
      forwarded <- Ref.of[IO, List[List[Path]]](Nil)
      _ <- DesktopHooks.install(FakeDesktop(), gui = true).use { hooks =>
        DesktopHooks.forwardQueued(hooks, paths => forwarded.update(_ :+ paths).as(Delivery.Delivered), 50.millis)
      }
      seen <- forwarded.get
    yield seen
    program.unsafeRunSync() shouldBe Nil
  }

  "A quit request from the desktop" should "perform the quit once the orderly quit completes" in {
    val program = answersAndResponse.flatMap((answers, response) =>
      DesktopHooks.answer(response, IO.pure(QuitOutcome.Completed)) >> answers.get
    )
    program.unsafeRunSync() shouldBe List("perform")
  }

  it should "cancel the quit when the orderly quit was called off" in {
    val program = answersAndResponse.flatMap((answers, response) =>
      DesktopHooks.answer(response, IO.pure(QuitOutcome.Abandoned)) >> answers.get
    )
    program.unsafeRunSync() shouldBe List("cancel")
  }

  it should "cancel the quit when the orderly quit fails, rather than leave the desktop waiting" in {
    val program = answersAndResponse.flatMap((answers, response) =>
      DesktopHooks.answer(response, IO.raiseError(new RuntimeException("save failed"))) >> answers.get
    )
    program.unsafeRunSync() shouldBe List("cancel")
  }

  it should "run the orderly quit once for each request the desktop raises" in {
    val desktop = FakeDesktop()
    val program = for
      (answers, response) <- answersAndResponse
      quits               <- Ref.of[IO, Int](0)
      _ <- DesktopHooks.install(desktop, gui = true).use { hooks =>
        val serve = DesktopHooks.serveQuitRequests(hooks.quitRequests, quits.update(_ + 1).as(QuitOutcome.Completed))
        serve.background.use(_ => desktop.raiseQuit(response) >> AwaitCondition.awaitValue(answers.get)(_.nonEmpty))
      }
      ran  <- quits.get
      seen <- answers.get
    yield (ran, seen)
    program.unsafeRunSync() shouldBe (1, List("perform"))
  }
