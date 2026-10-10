package com.serenity.state.manager

import java.nio.file.Path

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.lsp.config.LanguageId
import com.serenity.lsp.{DocumentDelta, LspEffect}
import com.serenity.rope.{Balance, ChangeSet, Rope}
import com.serenity.state.models.*
import com.serenity.state.reducers.{AppEffect, LspQueueEffect}
import com.serenity.testkit.RopeText
import com.serenity.testkit.VirtualTime.runVirtual
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** An edit reaches the LSP lane with the change it made and the versions it spans (#1838), and a change the editor
  * cannot describe reaches it with none, so the server is sent a comparison instead.
  */
class LspDocumentSyncDeltaSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val bufferId = BufferId(0)
  private val path     = Path.of("/workspace/Foo.scala")
  private val uri      = path.toUri.toString

  private def stateOf(document: Document): AppState =
    AppState.initial.copy(persisted =
      AppState.initial.persisted.copy(buffers = Map(bufferId -> Buffer(bufferId, document)))
    )

  private def initial: Document = Document(Rope("abc"), filePath = Some(path), language = Some(LanguageId.Scala))

  private def deltasOf(before: Document, after: Document): List[Option[DocumentDelta]] =
    (for
      sent <- Ref.of[IO, List[AppEffect]](Nil)
      port = LspDocumentSyncPort(
        currentState = IO.pure(stateOf(after)),
        interpretEffect = effect => sent.update(_ :+ effect),
        candidateLspBufferIds = (_, _) => Set(bufferId)
      )
      _       <- new LspDocumentSync(port).enqueueChangedLspDocuments(stateOf(before))
      effects <- sent.get
    yield effects.collect { case AppEffect.LspQueue(LspQueueEffect.DocumentChanged(_, _, _, delta)) => delta })
      .unsafeRunSync()

  "LspDocumentSync" should "queue an edit with its change and the versions it spans" in {
    val before = initial
    val after  = before.edited(Rope("abXc"), ChangeSet.single(3, 2, 2, "X"))

    deltasOf(before, after) shouldBe List(Some(DocumentDelta(ChangeSet.single(3, 2, 2, "X"), 0L, 1L)))
  }

  it should "queue a change it cannot describe with no delta" in {
    val before = initial

    deltasOf(before, before.withContent(Rope("abd"))) shouldBe List(None)
  }

  "The LSP queue" should "join the deltas of coalesced edits into one that spans them all" in {
    val first  = DocumentDelta(ChangeSet.single(3, 3, 3, "d"), 0L, 1L)
    val second = DocumentDelta(ChangeSet.single(4, 0, 0, "x"), 1L, 2L)
    val queued = runVirtual(
      LspEffectQueue.create.flatMap(queue =>
        queue.enqueueDocumentChange(uri, LanguageId.Scala, RopeText("abcd"), Some(first)) >>
          queue.enqueueDocumentChange(uri, LanguageId.Scala, RopeText("xabcd"), Some(second)) >>
          queue.stream.take(1).compile.toList
      )
    )

    queued shouldBe List(
      LspEffect.FileChanged(uri, LanguageId.Scala, RopeText("xabcd"), 2, first.andThen(second))
    )
    first.andThen(second).map(_.fromVersion) shouldBe Some(0L)
  }

  it should "drop the delta when any coalesced edit has none" in {
    val first  = DocumentDelta(ChangeSet.single(3, 3, 3, "d"), 0L, 1L)
    val second = DocumentDelta(ChangeSet.single(4, 0, 0, "x"), 1L, 2L)
    val queued = runVirtual(
      LspEffectQueue.create.flatMap(queue =>
        queue.enqueueDocumentChange(uri, LanguageId.Scala, RopeText("abcd"), Some(first)) >>
          queue.enqueueDocumentChange(uri, LanguageId.Scala, RopeText("ABCD"), None) >>
          queue.enqueueDocumentChange(uri, LanguageId.Scala, RopeText("xABCD"), Some(second)) >>
          queue.stream.take(1).compile.toList
      )
    )

    queued shouldBe List(LspEffect.FileChanged(uri, LanguageId.Scala, RopeText("xABCD"), 2, None))
  }

  it should "drop the delta when the edits are not consecutive versions" in {
    val first = DocumentDelta(ChangeSet.single(3, 3, 3, "d"), 0L, 1L)
    val gap   = DocumentDelta(ChangeSet.single(4, 0, 0, "x"), 5L, 6L)
    val queued = runVirtual(
      LspEffectQueue.create.flatMap(queue =>
        queue.enqueueDocumentChange(uri, LanguageId.Scala, RopeText("abcd"), Some(first)) >>
          queue.enqueueDocumentChange(uri, LanguageId.Scala, RopeText("xabcd"), Some(gap)) >>
          queue.stream.take(1).compile.toList
      )
    )

    queued shouldBe List(LspEffect.FileChanged(uri, LanguageId.Scala, RopeText("xabcd"), 2, None))
  }
