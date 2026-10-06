package com.serenity.state.manager

import java.lang.management.ManagementFactory
import java.nio.file.Path
import java.util.concurrent.Executors

import scala.concurrent.ExecutionContext

import cats.effect.IO
import cats.effect.unsafe.{IORuntime, IORuntimeConfig}
import com.serenity.lsp.LspEffect
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.state.reducers.{AppEffect, LspQueueEffect}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** What a content-changing dispatch costs the LSP lane. Every keystroke in a code workspace reaches
  * `LspDocumentSync.enqueueChangedLspDocuments`, and the lane keeps only the latest text of a document, so the cost of
  * an edit must not depend on how large the document is: the rope is handed over as it is, and the text is only
  * collected when a server is actually sent it.
  *
  * Measured the way [[com.serenity.state.undo.UndoStatePerformanceSpec]] measures allocation, on a runtime whose
  * compute pool is one thread, so the thread counter read before and after is the thread that ran the work.
  */
class LspDocumentSyncPerformanceSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val isolated: IORuntime =
    val (scheduler, _) = IORuntime.createDefaultScheduler("lsp-sync-scheduler")
    def singleThread(name: String) =
      ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor { task =>
        val thread = new Thread(task, name)
        thread.setDaemon(true)
        thread
      })
    IORuntime(
      singleThread("lsp-sync-compute"),
      singleThread("lsp-sync-blocking"),
      scheduler,
      () => (),
      IORuntimeConfig()
    )

  private val allocationBean = ManagementFactory.getThreadMXBean match
    case bean: com.sun.management.ThreadMXBean if bean.isThreadAllocatedMemorySupported =>
      if !bean.isThreadAllocatedMemoryEnabled then bean.setThreadAllocatedMemoryEnabled(true)
      Some(bean)
    case _ => None

  private val bufferId = BufferId(0)
  private val path     = Path.of("/workspace/Foo.scala")

  private def stateWith(content: com.serenity.rope.Rope): AppState =
    val document = Document(content, filePath = Some(path), language = Some(LanguageId.Scala))
    AppState.initial.copy(persisted =
      AppState.initial.persisted.copy(buffers = Map(bufferId -> Buffer(bufferId, document)))
    )

  /** The state before and after one character is typed into the middle of a document of `size` characters. */
  private def edit(size: Int): (AppState, AppState) =
    val text   = "0123456789abcde\n" * (size / 16)
    val before = com.serenity.rope.Rope(text)
    val after  = before.insert(before.weight / 2, "x").getOrElse(before)
    (stateWith(before), stateWith(after))

  /** The dispatch-side half of the LSP lane for one edit: what the pipeline does after a commit, ending in the queue.
    */
  private def dispatch(queue: LspEffectQueue, before: AppState, after: AppState): IO[Unit] =
    val port = LspDocumentSyncPort(
      currentState = IO.pure(after),
      interpretEffect = {
        case AppEffect.LspQueue(LspQueueEffect.DocumentChanged(uri, languageId, text)) =>
          queue.enqueueDocumentChange(uri, languageId, text)
        case _ => IO.unit
      },
      candidateLspBufferIds = (_, _) => Set(bufferId)
    )
    new LspDocumentSync(port).enqueueChangedLspDocuments(before)

  /** Bytes one dispatch of the edit allocates once warmed up, measured on the isolated compute thread. */
  private def allocatedByOneEdit(size: Int): Option[Long] =
    allocationBean.map { bean =>
      val (before, after) = edit(size)
      val program =
        for
          queue    <- LspEffectQueue.create
          threadId <- IO(Thread.currentThread().threadId())
          _        <- dispatch(queue, before, after).replicateA_(2_000)
          start    <- IO(bean.getThreadAllocatedBytes(threadId))
          _        <- dispatch(queue, before, after)
          end      <- IO(bean.getThreadAllocatedBytes(threadId))
        yield end - start
      program.unsafeRunSync()(using isolated)
    }

  "Enqueueing an edit" should "cost the same for a 1 MB document as for a 10 KB one" in {
    withClue("the fixture must be a code workspace, or nothing is enqueued: ")(
      edit(16)._2.editingContext.hasCodeTooling shouldBe true
    )
    (allocatedByOneEdit(10 * 1024), allocatedByOneEdit(1024 * 1024)) match
      case (Some(small), Some(large)) =>
        withClue(s"10 KB document allocated ${small}B, 1 MB document allocated ${large}B: ") {
          large should be < small * 2L
        }
      case _ =>
        info("JVM per-thread allocation counter unsupported on this runtime -- skipping the allocation assertion")
  }

  it should "hand the lane the edited rope itself, not a copy of its text" in {
    val (before, after) = edit(1024)
    val queue           = LspEffectQueue.create.unsafeRunSync()(using isolated)
    dispatch(queue, before, after).unsafeRunSync()(using isolated)

    val queued = queue.stream.take(1).compile.toList.unsafeRunSync()(using isolated)
    queued.collect { case LspEffect.FileChanged(_, _, text, _) => text } shouldBe
      List(after.persisted.buffers(bufferId).document.content)
  }
