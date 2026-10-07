package com.serenity

import java.lang.management.ManagementFactory
import java.util.concurrent.Executors

import scala.concurrent.ExecutionContext

import cats.effect.IO
import cats.effect.unsafe.{IORuntime, IORuntimeConfig}
import cats.syntax.all.*
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.models.*
import com.serenity.ui.accessibility.{AccessibilityPublishGate, AccessibilitySnapshot, AccessibilitySync}
import com.serenity.ui.layout.{CellMetrics, ViewportSize}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Deterministic guard for the per-frame accessibility work: the bytes allocated to sync and gate a snapshot after a
  * one-character edit must not grow with the size of the document. Allocation is counted per thread, as in
  * `UndoStatePerformanceSpec`, so the measurement does not depend on how fast or busy the machine is; the work runs on
  * a single-thread runtime of its own (as in `IoBudgetMeasurementSpec`) so no other suite's threads are involved.
  */
class AccessibilityAllocationSpec extends AnyFlatSpec with Matchers:
  given Balance = Balance.default

  private val viewport = ViewportSize(100, 30)
  private val metrics  = CellMetrics(8, 16, 12)
  private val warmUp   = 30
  private val measured = 300

  private val allocationBean = ManagementFactory.getThreadMXBean match
    case bean: com.sun.management.ThreadMXBean if bean.isThreadAllocatedMemorySupported =>
      if !bean.isThreadAllocatedMemoryEnabled then bean.setThreadAllocatedMemoryEnabled(true)
      Some(bean)
    case _ => None

  private val isolated: IORuntime =
    val executor = Executors.newSingleThreadExecutor { task =>
      val thread = new Thread(task, "accessibility-allocation")
      thread.setDaemon(true)
      thread
    }
    val (scheduler, _) = IORuntime.createDefaultScheduler("accessibility-allocation-scheduler")
    IORuntime(
      ExecutionContext.fromExecutor(executor),
      ExecutionContext.fromExecutor(executor),
      scheduler,
      () => (),
      IORuntimeConfig()
    )

  private def stateShowing(content: Rope): AppState =
    val buffer = Buffer(BufferId(1), Document(content))
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

  private def documentOf(characters: Int): Rope = Rope("0123456789abcde\n" * (characters / 16))

  /** The states a typist walks through: each is the previous document with one more character at its start. */
  private def keystrokes(start: Rope, count: Int): List[AppState] =
    List.unfold((start, count)) {
      case (_, 0) => None
      case (rope, remain) =>
        val next = rope.insert(0, "x").getOrElse(rope)
        Some((stateShowing(next), (next, remain - 1)))
    }

  /** The median bytes allocated by one frame's accessibility work (sync, then the publish gate) per edit. */
  private def medianBytesPerEdit(characters: Int): Option[Long] =
    allocationBean.map { bean =>
      val states = keystrokes(documentOf(characters), warmUp + measured)
      val program =
        for
          sync     <- AccessibilitySync.empty
          threadId <- IO(Thread.currentThread().threadId())
          gate = new AccessibilityPublishGate
          samples <- states.traverse { state =>
            for
              before   <- IO(bean.getThreadAllocatedBytes(threadId))
              snapshot <- sync.sync(state)(previous => IO(AccessibilitySnapshot.from(state, viewport, previous)))
              _        <- IO(gate.admit(snapshot, metrics))
              after    <- IO(bean.getThreadAllocatedBytes(threadId))
            yield after - before
          }
          ordered = samples.drop(warmUp).sorted
        yield ordered.drop(ordered.size / 2).headOption.getOrElse(0L)
      program.unsafeRunSync()(using isolated)
    }

  "Syncing accessibility after a one-character edit" should "allocate independently of the document's size" in {
    (medianBytesPerEdit(10 * 1024), medianBytesPerEdit(1024 * 1024)) match
      case (Some(small), Some(large)) =>
        withClue(s"10 KB document allocated ${small}B per sync, 1 MB document allocated ${large}B: ") {
          large.toDouble should be < small.toDouble * 2.0
        }
      case _ =>
        info("JVM per-thread allocation counter unsupported on this runtime -- skipping the allocation assertion")
  }
