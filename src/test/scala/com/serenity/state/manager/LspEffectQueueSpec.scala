package com.serenity.state.manager

import scala.concurrent.duration.*

import cats.effect.IO
import com.serenity.lsp.LspEffect
import com.serenity.lsp.config.LanguageId
import com.serenity.state.models.{CursorPosition, NoticePromptId}
import com.serenity.testkit.RopeText
import com.serenity.testkit.VirtualTime.runVirtual
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** `LspEffectQueue` is the LSP lane (#1697 Wave 3): one FIFO feeding `LspManager`'s single consumer, so every server
  * sees its documents' notifications and requests in the order the editor produced them. Consecutive edits to one
  * document coalesce into one `didChange` carrying the latest text.
  */
class LspEffectQueueSpec extends AnyFlatSpec with Matchers:

  private val scalaUri  = "file:///project/Main.scala"
  private val pythonUri = "file:///project/main.py"
  private val anchor    = CursorPosition(0, 0)

  private def drained(enqueue: LspEffectQueue => IO[Unit], count: Int): List[LspEffect] =
    runVirtual(LspEffectQueue.create.flatMap(queue => enqueue(queue) >> queue.stream.take(count).compile.toList))

  "The LSP queue" should "coalesce consecutive edits to a document into one change with the latest text" in {
    drained(
      queue =>
        queue.enqueue(LspEffect.FileOpened(scalaUri, LanguageId.Scala, RopeText("a"))) >>
          queue.enqueueDocumentChange(scalaUri, LanguageId.Scala, RopeText("ab")) >>
          queue.enqueueDocumentChange(scalaUri, LanguageId.Scala, RopeText("abc")),
      2
    ) shouldBe List(
      LspEffect.FileOpened(scalaUri, LanguageId.Scala, RopeText("a")),
      LspEffect.FileChanged(scalaUri, LanguageId.Scala, RopeText("abc"), 2)
    )
  }

  it should "keep each server's effects in the order they were enqueued when servers interleave" in {
    drained(
      queue =>
        queue.enqueue(LspEffect.FileOpened(scalaUri, LanguageId.Scala, RopeText("s"))) >>
          queue.enqueue(LspEffect.FileOpened(pythonUri, LanguageId.Python, RopeText("p"))) >>
          queue.enqueueDocumentChange(scalaUri, LanguageId.Scala, RopeText("s1")) >>
          queue.enqueue(LspEffect.HoverRequested(pythonUri, LanguageId.Python, 0, 0, anchor)) >>
          queue.enqueueDocumentChange(pythonUri, LanguageId.Python, RopeText("p1")) >>
          queue.enqueue(LspEffect.FileClosed(scalaUri, LanguageId.Scala)),
      6
    ) shouldBe List(
      LspEffect.FileOpened(scalaUri, LanguageId.Scala, RopeText("s")),
      LspEffect.FileOpened(pythonUri, LanguageId.Python, RopeText("p")),
      LspEffect.FileChanged(scalaUri, LanguageId.Scala, RopeText("s1"), 2),
      LspEffect.HoverRequested(pythonUri, LanguageId.Python, 0, 0, anchor),
      LspEffect.FileChanged(pythonUri, LanguageId.Python, RopeText("p1"), 2),
      LspEffect.FileClosed(scalaUri, LanguageId.Scala)
    )
  }

  it should "not fold an edit into a change queued before a request on the same document" in {
    drained(
      queue =>
        queue.enqueue(LspEffect.FileOpened(scalaUri, LanguageId.Scala, RopeText("a"))) >>
          queue.enqueueDocumentChange(scalaUri, LanguageId.Scala, RopeText("ab")) >>
          queue.enqueue(LspEffect.HoverRequested(scalaUri, LanguageId.Scala, 0, 2, anchor)) >>
          queue.enqueueDocumentChange(scalaUri, LanguageId.Scala, RopeText("abc")),
      4
    ) shouldBe List(
      LspEffect.FileOpened(scalaUri, LanguageId.Scala, RopeText("a")),
      LspEffect.FileChanged(scalaUri, LanguageId.Scala, RopeText("ab"), 2),
      LspEffect.HoverRequested(scalaUri, LanguageId.Scala, 0, 2, anchor),
      LspEffect.FileChanged(scalaUri, LanguageId.Scala, RopeText("abc"), 3)
    )
  }

  it should "send an edit made after a language switch to the new server, not the old one" in {
    drained(
      queue =>
        queue.enqueue(LspEffect.FileOpened(scalaUri, LanguageId.Scala, RopeText("a"))) >>
          queue.enqueueDocumentChange(scalaUri, LanguageId.Scala, RopeText("ab")) >>
          queue.enqueue(LspEffect.FileClosed(scalaUri, LanguageId.Scala)) >>
          queue.enqueue(LspEffect.FileOpened(scalaUri, LanguageId.Python, RopeText("ab"))) >>
          queue.enqueueDocumentChange(scalaUri, LanguageId.Python, RopeText("abc")),
      5
    ) shouldBe List(
      LspEffect.FileOpened(scalaUri, LanguageId.Scala, RopeText("a")),
      LspEffect.FileChanged(scalaUri, LanguageId.Scala, RopeText("ab"), 2),
      LspEffect.FileClosed(scalaUri, LanguageId.Scala),
      LspEffect.FileOpened(scalaUri, LanguageId.Python, RopeText("ab")),
      LspEffect.FileChanged(scalaUri, LanguageId.Python, RopeText("abc"), 2)
    )
  }

  it should "keep delivering a document's edits after a consumer is cancelled as a change arrives" in {
    val cancelPoints = (0 to 3).toList
    val nextTexts = cancelPoints.map { cedesBeforeCancel =>
      runVirtual(
        for
          queue    <- LspEffectQueue.create
          _        <- queue.enqueue(LspEffect.FileOpened(scalaUri, LanguageId.Scala, RopeText("a")))
          _        <- queue.stream.take(1).compile.drain
          consumer <- queue.stream.take(1).compile.drain.start
          _        <- IO.cede
          _        <- queue.enqueueDocumentChange(scalaUri, LanguageId.Scala, RopeText("ab"))
          _        <- IO.cede.replicateA_(cedesBeforeCancel)
          _        <- consumer.cancel
          _        <- queue.enqueueDocumentChange(scalaUri, LanguageId.Scala, RopeText("abc"))
          next     <- queue.stream.take(1).compile.toList.timeout(1.second).attempt
        yield next.map(_.collect { case LspEffect.FileChanged(_, _, text, _) => text.collect() })
      )
    }

    nextTexts shouldBe cancelPoints.map(_ => Right(List("abc")))
  }

  it should "carry the answer to a server's question without cutting short the edits to any document" in {
    drained(
      queue =>
        queue.enqueue(LspEffect.FileOpened(scalaUri, LanguageId.Scala, RopeText("a"))) >>
          queue.enqueueDocumentChange(scalaUri, LanguageId.Scala, RopeText("ab")) >>
          queue.enqueue(LspEffect.MessageRequestAnswered(NoticePromptId(1), Some(0))) >>
          queue.enqueueDocumentChange(scalaUri, LanguageId.Scala, RopeText("abc")),
      3
    ) shouldBe List(
      LspEffect.FileOpened(scalaUri, LanguageId.Scala, RopeText("a")),
      LspEffect.FileChanged(scalaUri, LanguageId.Scala, RopeText("abc"), 2),
      LspEffect.MessageRequestAnswered(NoticePromptId(1), Some(0))
    )
  }
