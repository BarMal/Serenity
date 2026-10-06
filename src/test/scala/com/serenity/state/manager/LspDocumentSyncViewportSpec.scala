package com.serenity.state.manager

import java.nio.file.Path

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.lsp.LspEffect
import com.serenity.lsp.config.LanguageId
import com.serenity.rope.{Balance, Rope}
import com.serenity.state.models.*
import com.serenity.state.reducers.{AppEffect, LspQueueEffect}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A server that answers semantic tokens only for a range has to be told which lines the editor shows (#1837). */
class LspDocumentSyncViewportSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val bufferId = BufferId(0)
  private val path     = Path.of("/workspace/Foo.scala")
  private val uri      = path.toUri.toString

  private def stateWith(content: String, viewport: Viewport): AppState =
    val document = Document(Rope(content), filePath = Some(path), language = Some(LanguageId.Scala))
    AppState.initial.copy(persisted =
      AppState.initial.persisted.copy(buffers = Map(bufferId -> Buffer(bufferId, document, viewport = viewport)))
    )

  private def effectsOf(before: AppState, after: AppState): List[AppEffect] =
    (for
      sent <- Ref.of[IO, List[AppEffect]](Nil)
      port = LspDocumentSyncPort(
        currentState = IO.pure(after),
        interpretEffect = effect => sent.update(_ :+ effect),
        candidateLspBufferIds = (_, _) => Set(bufferId)
      )
      _       <- new LspDocumentSync(port).enqueueChangedLspDocuments(before)
      effects <- sent.get
    yield effects).unsafeRunSync()

  private val document = (1 to 300).map(n => s"line $n").mkString("\n")

  private val top = Viewport(topLine = 0, visibleLines = 30, visibleColumns = 80)

  "LspDocumentSync" should "announce the visible lines when the viewport scrolls" in {
    val before = stateWith(document, top)
    val after  = stateWith(document, top.copy(topLine = 100))

    effectsOf(before, after) shouldBe List(
      AppEffect.LspQueue(
        LspQueueEffect.Enqueue(LspEffect.VisibleRangeChanged(uri, LanguageId.Scala, 100, 129))
      )
    )
  }

  it should "announce the visible lines when the viewport is resized" in {
    val before = stateWith(document, top)
    val after  = stateWith(document, top.copy(visibleLines = 50))

    effectsOf(before, after) shouldBe List(
      AppEffect.LspQueue(
        LspQueueEffect.Enqueue(LspEffect.VisibleRangeChanged(uri, LanguageId.Scala, 0, 49))
      )
    )
  }

  it should "stop the visible lines at the end of the document" in {
    val before = stateWith(document, top)
    val after  = stateWith(document, top.copy(topLine = 290))

    effectsOf(before, after) shouldBe List(
      AppEffect.LspQueue(
        LspQueueEffect.Enqueue(LspEffect.VisibleRangeChanged(uri, LanguageId.Scala, 290, 299))
      )
    )
  }

  it should "announce nothing when neither the content nor the viewport changed" in {
    val state = stateWith(document, top)

    effectsOf(state, state) shouldBe Nil
  }

  it should "announce both the new text and the visible lines when an edit scrolls the viewport" in {
    val before = stateWith(document, top)
    val after  = stateWith(document + "\nc", top.copy(topLine = 3))

    val effects = effectsOf(before, after)

    effects.collect {
      case AppEffect.LspQueue(LspQueueEffect.DocumentChanged(changed, _, text)) =>
        (changed, text.collect())
    } shouldBe List((uri, document + "\nc"))
    effects.drop(1) shouldBe List(
      AppEffect.LspQueue(LspQueueEffect.Enqueue(LspEffect.VisibleRangeChanged(uri, LanguageId.Scala, 3, 32)))
    )
  }
