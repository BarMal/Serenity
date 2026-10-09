package com.serenity.lsp

import java.lang.ref.WeakReference

import scala.concurrent.duration.*

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import com.serenity.keystroke.events.LspEvent
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.models.CursorPosition
import com.serenity.testkit.AwaitCondition.awaitValue
import fs2.Stream
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** What `LspManager` holds on to between edits. A document with no server has nobody to send its text to, so a large
  * file edited all day must not leave a copy of itself behind; one with a server keeps just the latest text, the base
  * for its next `didChange`.
  */
class LspManagerDocumentRetentionSpec extends AnyFlatSpec with Matchers with LspManagerSpecFixture:

  given Balance = Balance.default

  private val anchor = CursorPosition(0, 1)

  /** Offers a document and `edits` changes to it, returning weak references to the texts and holding none of them. */
  private def offerEdits(manager: Harness, edits: Int): IO[List[WeakReference[Rope]]] =
    (0 to edits).toList.traverse { version =>
      IO(Rope(s"object Foo$version")).flatMap { text =>
        val effect =
          if version == 0 then LspEffect.FileOpened(uri, LanguageId.Scala, text)
          else LspEffect.FileChanged(uri, LanguageId.Scala, text, version + 1)
        manager.effects.offer(Some(effect)).as(new WeakReference(text))
      }
    }

  private def retained(references: List[WeakReference[Rope]]): IO[Int] =
    IO(System.gc()) >> IO.sleep(20.millis) >> IO(references.count(_.get != null))

  private def hoverAnswered(manager: Harness): IO[Unit] =
    manager.effects.offer(Some(LspEffect.HoverRequested(uri, LanguageId.Scala, 0, 1, anchor))) >>
      awaitValue(manager.events.get)(_.exists {
        case LspEvent.LspHoverReceived(_, _) => true
        case _                               => false
      }).void

  "LspManager" should "retain no text for a document that no server serves" in {
    val retainedTexts = harness(serverAvailable = false)
      .use { manager =>
        for
          references <- offerEdits(manager, 100)
          _          <- hoverAnswered(manager)
          remaining  <- retained(references).iterateUntil(_ == 0).timeout(30.seconds).attempt
        yield remaining.fold(_ => references.count(_.get != null), identity)
      }
      .unsafeRunSync()

    retainedTexts shouldBe 0
  }

  it should "retain only the latest text of a document its server is serving" in {
    val retainedTexts = harness()
      .use { manager =>
        for
          references <- offerEdits(manager, 40)
          _          <- manager.effects.offer(Some(LspEffect.HoverRequested(uri, LanguageId.Scala, 0, 1, anchor)))
          _ <- Stream
            .repeatEval(takeMessage(manager.connection))
            .exists(_.hcursor.downField("method").as[String].contains("textDocument/hover"))
            .compile
            .drain
            .timeout(30.seconds)
          remaining <- retained(references).iterateUntil(_ <= 1).timeout(30.seconds).attempt
        yield remaining.fold(_ => references.count(_.get != null), identity)
      }
      .unsafeRunSync()

    retainedTexts shouldBe 1
  }
