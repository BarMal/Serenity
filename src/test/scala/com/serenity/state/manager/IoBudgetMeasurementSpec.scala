package com.serenity.state.manager

import java.lang.management.ManagementFactory
import java.nio.file.{Files, Path}
import java.util.concurrent.Executors

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

import cats.effect.unsafe.implicits.global
import cats.effect.unsafe.{IORuntime, IORuntimeConfig}
import cats.effect.{IO, Ref}
import cats.syntax.all.*
import com.serenity.app.AppRuntime
import com.serenity.io.{AtomicFileWriter, FileChangeWatcher, FileManager}
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

  // `/proc/self/io` counts the whole JVM, and sbt runs suites in parallel in one JVM, so another suite's file I/O would
  // land in these budgets. The counters of a single thread are private to it: every operation is measured on a runtime
  // whose compute and blocking pools are one thread each, and only those two threads' counters are read.
  private def singleThread(name: String): ExecutionContext =
    ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor { task =>
      val thread = new Thread(task, name)
      thread.setDaemon(true)
      thread
    })

  private val isolated: IORuntime =
    val (scheduler, _) = IORuntime.createDefaultScheduler("io-budget-scheduler")
    IORuntime(
      singleThread("io-budget-compute"),
      singleThread("io-budget-blocking"),
      scheduler,
      () => (),
      IORuntimeConfig()
    )

  private def currentThreadIo(): (Long, Long) =
    val lines               = Files.readAllLines(Path.of("/proc/thread-self/io")).asScala
    def field(name: String) = lines.find(_.startsWith(name + ":")).map(_.split(":")(1).trim.toLong).getOrElse(0L)
    (field("rchar"), field("wchar"))

  private val ioOfMeasuredThreads: IO[(Long, Long)] =
    for
      (computeRead, computeWritten)   <- IO(currentThreadIo())
      (blockingRead, blockingWritten) <- IO.blocking(currentThreadIo())
    yield (computeRead + blockingRead, computeWritten + blockingWritten)

  private def measure[A](label: String, runs: Int)(op: IO[A]): IoCost =
    val program =
      for
        _        <- op
        (r0, w0) <- ioOfMeasuredThreads
        started  <- IO.monotonic
        _        <- op.replicateA_(runs)
        elapsed  <- IO.monotonic.map(_ - started)
        (r1, w1) <- ioOfMeasuredThreads
        cost = IoCost((r1 - r0) / runs, (w1 - w0) / runs)
        _ <- IO(
          info(
            f"[IO-BUDGET] $label%-40s read/op=${cost.read}%,12d B  written/op=${cost.written}%,12d B  time/op=${elapsed.toNanos / runs / 1e6}%8.2f ms"
          )
        )
      yield cost
    program.unsafeRunSync()(using isolated)

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

  // Allocation is counted per thread too, for the same reason as the I/O counters.
  private val threadMemory = ManagementFactory.getThreadMXBean match
    case sun: com.sun.management.ThreadMXBean => Some(sun)
    case _                                    => None

  private def allocatedByCurrentThread(): Long =
    threadMemory.fold(0L)(_.getThreadAllocatedBytes(Thread.currentThread.threadId))

  private val allocatedByMeasuredThreads: IO[Long] =
    for
      compute  <- IO(allocatedByCurrentThread())
      blocking <- IO.blocking(allocatedByCurrentThread())
    yield compute + blocking

  private def allocationPerOp[A](runs: Int)(op: IO[A]): Long =
    val program =
      for
        _      <- op
        before <- allocatedByMeasuredThreads
        _      <- op.replicateA_(runs)
        after  <- allocatedByMeasuredThreads
      yield (after - before) / runs
    program.unsafeRunSync()(using isolated)

  private val DirtyBytes = 10 * 1024

  // What the file API itself charges for the operations a save cannot avoid. A path is resolved to a native string and
  // a stat result is built on every call, and Windows allocates several times what Linux does for both, so a budget
  // fixed in bytes on one platform fails on the other without any encoding having crept back in. Budgets are the
  // encoding allowance plus this, measured where the test runs. Linux keeps its exact bound: the allowance is zero there.
  private val StatsPerSave = 16

  private val platformChargesFileApi =
    val osName = System.getProperty("os.name", "").toLowerCase
    osName.startsWith("windows") || osName.startsWith("mac")

  private def fileApiAllocation(atomicWrites: Int): Long =
    if platformChargesFileApi then measuredFileApiAllocation(atomicWrites) else 0L

  private def measuredFileApiAllocation(atomicWrites: Int): Long =
    val directory = Files.createTempDirectory("io-budget-file-api")
    val file      = Files.writeString(directory.resolve("probe.txt"), "probe")
    val payload   = Array.fill[Byte](DirtyBytes)('d')
    val stats = IO.blocking {
      (1 to StatsPerSave).foreach { _ =>
        Files.isSymbolicLink(file)
        Files.isRegularFile(file)
        Files.exists(file)
      }
    }
    val writes =
      (1 to atomicWrites).toList.traverse_(n => AtomicFileWriter.writeBytes(directory.resolve(s"w$n"), payload))
    allocationPerOp(20)(stats >> writes)

  // One dirty buffer of `DirtyBytes` among `cleanBuffers` saved 1 MB files that are merely open.
  private def editIdleSessionState(
    cleanBuffers: Int,
    dirtyText: String = "d" * DirtyBytes,
    cleanText: String = "0123456789abcde\n" * 65536
  ): AppState =
    val initial = AppState.initial
    val dirtyId = initial.persisted.bufferOrder.head
    val dirty   = Buffer.fromString(dirtyId, dirtyText)
    val clean =
      (1 to cleanBuffers).toList.map(n => Buffer.fromFile(BufferId(1000 + n), Path.of(s"/clean/$n.txt"), cleanText))
    initial.copy(persisted =
      initial.persisted.copy(
        buffers = (dirty.copy(document = dirty.document.copy(isDirty = true)) :: clean).map(b => b.id -> b).toMap,
        bufferOrder = dirtyId :: clean.map(_.id)
      )
    )

  private def newSession(): SessionManager =
    SessionManager.create(
      Files.createTempDirectory("io-budget-edit-idle"),
      AppThemeManager.create,
      NoOpLogger.impl[IO],
      SessionManager.SessionPolicy.interactive
    )

  "IO budget" should "be measured for file saves and focus-in" in {
    assume(countersAvailable)
    List(1, 10).foreach { megabytes =>
      val path = Files.createTempFile("io-budget", ".txt")
      Files.writeString(path, "0123456789abcde\n" * (megabytes * 65536))
      val manager = new FileManager()
      val opened  = manager.loadFile(path, bufferId).unsafeRunSync()
      val saved   = Ref.of[IO, Buffer](opened).unsafeRunSync()
      val save    = measure(s"file save ${megabytes}MB", 5)(saved.get.flatMap(manager.saveBuffer).flatMap(saved.set))
      val fixture = harness(focused(saved.get.unsafeRunSync()))
      val focusIn = measure(s"focus-in check ${megabytes}MB", 5)(fixture.handlers.observeFocusedExternalRevisionEffect)
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
      val cost = measure(s"session save ${megabytes}MB unsaved text", 5)(session.saveSession(state))
      if megabytes == 10 then
        withClue("the payload is written once, not twice: ")(cost.written should be < (megabytes * Megabyte * 3 / 2))
    }
  }

  it should "keep an edit-idle session save to the bytes of what was edited" in {
    assume(countersAvailable)
    val session = newSession()
    val state   = editIdleSessionState(cleanBuffers = 9)
    val steady  = measure("edit-idle save, 1 dirty 10KB + 9 clean 1MB", 10)(session.saveSession(state))
    val edits   = Ref.unsafe[IO, Int](0)
    val typing = measure("edit-idle save after an edit", 10)(
      edits.updateAndGet(_ + 1).flatMap(n => session.saveSession(editIdleSessionState(9, s"$n" * DirtyBytes)))
    )
    withClue("clean buffers are not read: ")(steady.read should be < 64L * 1024)
    withClue("an unchanged dirty buffer is not written again: ")(steady.written should be < 64L * 1024)
    withClue("an edited dirty buffer is written once, beside the session JSON: ")(
      typing.written should be < DirtyBytes + 64L * 1024
    )
    withClue("clean buffers are not read after an edit either: ")(typing.read should be < 64L * 1024)
  }

  it should "allocate independently of the size of the clean buffers on an edit-idle session save" in {
    assume(threadMemory.exists(_.isThreadAllocatedMemorySupported))
    val tiny  = allocationPerOp(10)(newSession().saveSession(editIdleSessionState(cleanBuffers = 9, cleanText = "x\n")))
    val large = allocationPerOp(10)(newSession().saveSession(editIdleSessionState(cleanBuffers = 9)))
    val absent = allocationPerOp(10)(newSession().saveSession(editIdleSessionState(cleanBuffers = 0)))
    info(
      f"[ALLOC-BUDGET] edit-idle save alloc/op: 0 clean=$absent%,d B  9 clean 2B=$tiny%,d B  9 clean 1MB=$large%,d B"
    )
    withClue("nine open megabytes cost the save nothing beyond nine tiny buffers: ")(
      large - tiny should be < 16L * 1024
    )
    withClue("a clean buffer costs its session entry, not its text: ")((large - absent) / 9 should be < 16L * 1024)
  }

  it should "allocate next to nothing on a session save that has nothing to write" in {
    assume(threadMemory.exists(_.isThreadAllocatedMemorySupported))
    val state = editIdleSessionState(cleanBuffers = 9)
    val steady =
      val session = newSession()
      allocationPerOp(20)(session.saveSession(state))
    val fileApi = fileApiAllocation(atomicWrites = 0)
    info(
      f"[ALLOC-BUDGET] session save with nothing to write: $steady%,d B/op, of which file API allowance $fileApi%,d B"
    )
    withClue("re-encoding the whole session state is what a no-change save used to cost: ")(
      steady should be < 32L * 1024 + fileApi
    )
  }

  it should "allocate a bounded amount on an edit-idle session save that has something to write" in {
    assume(threadMemory.exists(_.isThreadAllocatedMemorySupported))
    val session = newSession()
    val edits   = Ref.unsafe[IO, Int](0)
    // Built up front, so the measurement holds the save and not the building of the state it saves.
    val states  = Vector.tabulate(22)(n => editIdleSessionState(9, s"$n" * DirtyBytes, cleanText = "x\n"))
    val typing  = allocationPerOp(20)(edits.getAndUpdate(_ + 1).flatMap(n => session.saveSession(states(n))))
    val fileApi = fileApiAllocation(atomicWrites = 4)
    info(f"[ALLOC-BUDGET] edit-idle save after an edit: $typing%,d B/op, of which file API allowance $fileApi%,d B")
    withClue("the session's settings are encoded again on every edit unless they are unchanged: ")(
      typing should be < 384L * 1024 + fileApi
    )
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
