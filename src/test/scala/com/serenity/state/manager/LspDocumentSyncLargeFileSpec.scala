package com.serenity.state.manager

import java.nio.file.Path

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.config.AppConfig
import com.serenity.lsp.LspEffect
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.models.*
import com.serenity.state.reducers.{AppEffect, LspQueueEffect}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A document that outgrows the large-file limit is let go of by the language server, and taken up again when it comes
  * back under it (#1867).
  */
class LspDocumentSyncLargeFileSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val policy   = LargeFilePolicy(warnAboveChars = 10, toolingOffAboveChars = 20)
  private val bufferId = BufferId(0)
  private val path     = Path.of("/workspace/Foo.scala")
  private val uri      = path.toUri.toString

  private def documentOf(chars: Int): Document =
    Document(Rope("x" * chars), filePath = Some(path), language = Some(LanguageId.Scala))

  private def stateOf(document: Document): AppState =
    AppState.initial.copy(persisted =
      AppState.initial.persisted.copy(
        buffers = Map(bufferId -> Buffer(bufferId, document)),
        config = AppConfig.default.withLargeFilePolicy(policy)
      )
    )

  private def effectsOf(before: Document, after: Document): List[AppEffect] =
    (for
      sent <- Ref.of[IO, List[AppEffect]](Nil)
      port = LspDocumentSyncPort(
        currentState = IO.pure(stateOf(after)),
        interpretEffect = effect => sent.update(_ :+ effect),
        candidateLspBufferIds = (_, _) => Set(bufferId)
      )
      _       <- new LspDocumentSync(port).enqueueChangedLspDocuments(stateOf(before))
      effects <- sent.get
    yield effects).unsafeRunSync()

  "LspDocumentSync" should "keep syncing the edits of a large document under the tooling limit" in {
    val effects = effectsOf(documentOf(12), documentOf(15))

    effects.collect {
      case AppEffect.LspQueue(LspQueueEffect.DocumentChanged(changed, _, _, _)) =>
        changed
    } shouldBe List(uri)
  }

  it should "close a document on the server when an edit takes it past the tooling limit" in {
    effectsOf(documentOf(20), documentOf(21)) shouldBe
      List(AppEffect.LspQueue(LspQueueEffect.Enqueue(LspEffect.FileClosed(uri, LanguageId.Scala))))
  }

  it should "send nothing for the edits of a huge document" in {
    effectsOf(documentOf(30), documentOf(31)) shouldBe Nil
  }

  it should "announce a huge document afresh when an edit brings it back under the limit" in {
    effectsOf(documentOf(21), documentOf(20)).collect {
      case AppEffect.LspQueue(LspQueueEffect.Enqueue(LspEffect.FileOpened(opened, LanguageId.Scala, _))) => opened
    } shouldBe List(uri)
  }
