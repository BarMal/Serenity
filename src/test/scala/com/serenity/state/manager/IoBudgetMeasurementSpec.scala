package com.serenity.state.manager

import java.nio.file.{Files, Path}

import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import cats.syntax.all.*
import com.serenity.app.AppRuntime
import com.serenity.io.{FileChangeWatcher, FileManager}
import com.serenity.session.SessionManager
import com.serenity.state.models.*
import com.serenity.ui.theme.config.AppThemeManager
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.noop.NoOpLogger

class IoBudgetMeasurementSpec extends AnyFlatSpec with Matchers with StateManagerEffectHandlersHarness:

  // Per-process byte counters exist on Linux only; elsewhere the measurements are skipped rather than failed.
  private val countersAvailable = Files.exists(Path.of("/proc/self/io"))

  final private case class IoCost(read: Long, written: Long)

  private def procIo: (Long, Long) =
    val lines               = Files.readAllLines(Path.of("/proc/self/io")).toArray.map(_.toString)
    def field(name: String) = lines.find(_.startsWith(name + ":")).map(_.split(":")(1).trim.toLong).getOrElse(0L)
    (field("rchar"), field("wchar"))

  private def measure[A](label: String, runs: Int)(op: => A): IoCost =
    val _        = op
    val (r0, w0) = procIo
    val t0       = System.nanoTime()
    (1 to runs).foreach(_ => op)
    val t1       = System.nanoTime()
    val (r1, w1) = procIo
    info(
      f"[IO-BUDGET] $label%-40s read/op=${(r1 - r0) / runs}%,12d B  written/op=${(w1 - w0) / runs}%,12d B  time/op=${(t1 - t0) / runs / 1e6}%8.2f ms"
    )
    IoCost((r1 - r0) / runs, (w1 - w0) / runs)

  private def focused(buffer: Buffer): AppState =
    AppState.initial.copy(persisted =
      AppState.initial.persisted.copy(
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = AppState.initial.persisted.layout.copy(
          editorPanes = Map(PaneId(0) -> EditorPane.withBuffer(PaneId(0), buffer.id)),
          activeEditorPaneId = Some(PaneId(0))
        ),
        focus = Focus.EditorPane(PaneId(0))
      )
    )

  private val Megabyte = 1L << 20

  "IO budget" should "be measured for file saves and focus-in" in {
    assume(countersAvailable)
    List(1, 10).foreach { megabytes =>
      val path = Files.createTempFile("io-budget", ".txt")
      Files.writeString(path, "0123456789abcde\n" * (megabytes * 65536))
      val manager = new FileManager()
      val opened  = manager.loadFile(path, bufferId).unsafeRunSync()
      val saved   = Ref.of[IO, Buffer](opened).unsafeRunSync()
      val save = measure(s"file save ${megabytes}MB", 5) {
        saved.set(manager.saveBuffer(saved.get.unsafeRunSync()).unsafeRunSync()).unsafeRunSync()
      }
      val fixture = harness(focused(saved.get.unsafeRunSync()))
      val focusIn = measure(s"focus-in check ${megabytes}MB", 5) {
        fixture.handlers.observeFocusedExternalRevisionEffect.unsafeRunSync()
      }
      if megabytes == 10 then
        withClue("a save re-reads the file it replaces: ")(save.read should be < Megabyte)
        withClue("an unchanged file is stat-ed, not read, on focus-in: ")(focusIn.read should be < Megabyte)
    }
  }

  it should "be measured for session saves" in {
    assume(countersAvailable)
    List(1, 10).foreach { megabytes =>
      val root = Files.createTempDirectory("io-budget-session")
      val session =
        SessionManager.create(root, AppThemeManager.create, NoOpLogger.impl[IO], SessionManager.SessionPolicy())
      val initial = AppState.initial
      val id      = initial.persisted.bufferOrder.head
      val buffer  = Buffer.fromString(id, "0123456789abcde\n" * (megabytes * 65536))
      val state = initial.copy(persisted =
        initial.persisted.copy(buffers = Map(id -> buffer.copy(document = buffer.document.copy(isDirty = true))))
      )
      val cost = measure(s"session save ${megabytes}MB unsaved text", 5)(session.saveSession(state).unsafeRunSync())
      if megabytes == 10 then
        withClue("the payload is written once, not twice: ")(cost.written should be < (megabytes * Megabyte * 3 / 2))
    }
  }

  it should "count external-change watcher wakeups" in {
    // Its own directory, so only this spec's activity reaches the watcher.
    val file   = Files.createFile(Files.createTempDirectory("io-budget-watch").resolve("notes.txt"))
    val window = 10.seconds
    val count = (open: Map[Path, BufferId]) =>
      FileChangeWatcher.create.use { watcher =>
        for
          derivations <- Ref.of[IO, Int](0)
          _ <- AppRuntime
            .externalChangeWatchLoop(watcher, derivations.update(_ + 1).as(open), _ => IO.unit)
            .interruptAfter(window)
            .compile
            .drain
          n <- derivations.get
        yield n
      }
    val idle    = count(Map.empty).unsafeRunSync()
    val watched = count(Map(file -> BufferId(1))).unsafeRunSync()
    // The first derivation is the loop's start; every later one is a wakeup.
    val perMinute = (derivations: Int) => (derivations - 1).max(0) * (60.seconds / window).toInt
    info(s"[IO-BUDGET] watcher wakeups/min, nothing open: ${perMinute(idle)}")
    info(s"[IO-BUDGET] watcher wakeups/min, one file open, idle: ${perMinute(watched)}")
    perMinute(idle) shouldBe 0
    perMinute(watched) shouldBe 0
  }
