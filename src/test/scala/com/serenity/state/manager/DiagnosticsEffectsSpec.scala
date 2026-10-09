package com.serenity.state.manager

import java.nio.file.Path

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.command.DiagnosticsIntent
import com.serenity.diagnostics.RuntimeIdentity
import com.serenity.state.models.{ConfirmPrompt, Modal}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DiagnosticsEffectsSpec extends AnyFlatSpec with Matchers:

  private val identity = RuntimeIdentity.of("1.2.3", "abc1234", _ => None)
  private val logs     = Path.of(System.getProperty("java.io.tmpdir"), "serenity-logs")

  final private case class Fixture(effects: DiagnosticsEffects, calls: Ref[IO, Vector[String]])

  private def fixture(openFolder: Path => IO[Unit] = _ => IO.unit): Fixture =
    val calls = Ref.unsafe[IO, Vector[String]](Vector.empty)
    Fixture(
      DiagnosticsEffects(
        showModal = modal => calls.update(_ :+ s"modal:$modal"),
        identity = IO.pure(identity),
        logDirectory = logs,
        openFolder = path => calls.update(_ :+ s"open:$path") >> openFolder(path),
        copyText = text => calls.update(_ :+ s"copy:$text")
      ),
      calls
    )

  "DiagnosticsEffects" should "show the About prompt for this build" in {
    val f = fixture()

    f.effects.interpret(DiagnosticsIntent.ShowAbout).unsafeRunSync()

    f.calls.get.unsafeRunSync() shouldBe Vector(s"modal:${Modal.Confirm(ConfirmPrompt.about(identity))}")
  }

  it should "open the log directory" in {
    val f = fixture()

    f.effects.interpret(DiagnosticsIntent.OpenLogsFolder).unsafeRunSync()

    f.calls.get.unsafeRunSync() shouldBe Vector(s"open:$logs")
  }

  it should "say where the logs are when the folder cannot be opened" in {
    val f = fixture(_ => IO.raiseError(RuntimeException("no file manager")))

    f.effects.interpret(DiagnosticsIntent.OpenLogsFolder).unsafeRunSync()

    val calls = f.calls.get.unsafeRunSync()
    calls should have size 2
    calls.lastOption.getOrElse("") should include(logs.toString)
    calls.lastOption.getOrElse("") should include("modal:")
  }

  it should "copy the given text to the clipboard" in {
    val f = fixture()

    f.effects.interpret(DiagnosticsIntent.CopyToClipboard("details")).unsafeRunSync()

    f.calls.get.unsafeRunSync() shouldBe Vector("copy:details")
  }
