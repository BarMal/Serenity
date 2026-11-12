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

  private def fixture(
    openFolder: Path => IO[Unit] = _ => IO.unit,
    opensFolderInFileManager: Boolean = true
  ): Fixture =
    val calls = Ref.unsafe[IO, Vector[String]](Vector.empty)
    Fixture(
      DiagnosticsEffects(
        showModal = modal => calls.update(_ :+ s"modal:$modal"),
        identity = IO.pure(identity),
        logDirectory = logs,
        openFolder = path => calls.update(_ :+ s"open:$path") >> openFolder(path),
        copyText = text => calls.update(_ :+ s"copy:$text"),
        opensFolderInFileManager = IO.pure(opensFolderInFileManager),
        exportToTerminalClipboard = text => calls.update(_ :+ s"export:$text")
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

  "With no display to open a file manager on, Open Logs Folder" should "start no process" in {
    val f = fixture(opensFolderInFileManager = false)

    f.effects.interpret(DiagnosticsIntent.OpenLogsFolder).unsafeRunSync()

    f.calls.get.unsafeRunSync().filter(_.startsWith("open:")) shouldBe Nil
  }

  it should "show the path, and export it once to the terminal's clipboard" in {
    val f = fixture(opensFolderInFileManager = false)

    f.effects.interpret(DiagnosticsIntent.OpenLogsFolder).unsafeRunSync()

    val calls = f.calls.get.unsafeRunSync()
    calls.count(_ == s"export:$logs") shouldBe 1
    calls.filter(_.startsWith("copy:")) shouldBe Nil
    val modal = calls.filter(_.startsWith("modal:"))
    modal should have size 1
    modal.headOption.getOrElse("") should include(s"The logs folder is at $logs.")
  }

  it should "say honestly that the terminal may not take the copy, in a single outcome" in {
    val f = fixture(opensFolderInFileManager = false)

    f.effects.interpret(DiagnosticsIntent.OpenLogsFolder).unsafeRunSync()

    val modal = f.calls.get.unsafeRunSync().filter(_.startsWith("modal:")).mkString
    modal should include("Sent to your terminal's clipboard")
    modal should include("doesn't accept OSC 52")
    modal should not include "Could not open"
  }

  "DiagnosticsEffects.exported" should "queue the text for the clipboard sync, leaving the clipboard history alone" in {
    given com.serenity.rope.Balance = com.serenity.rope.Balance.default
    val before                      = com.serenity.state.models.AppState.initial
    val after                       = DiagnosticsEffects.exported("/tmp/logs")(before)

    after.runtime.clipboard shouldBe Some("/tmp/logs")
    after.runtime.clipboardExports shouldBe before.runtime.clipboardExports + 1
    after.runtime.clipboardHistory shouldBe before.runtime.clipboardHistory
  }

  it should "copy the given text to the clipboard" in {
    val f = fixture()

    f.effects.interpret(DiagnosticsIntent.CopyToClipboard("details")).unsafeRunSync()

    f.calls.get.unsafeRunSync() shouldBe Vector("copy:details")
  }
