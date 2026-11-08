package com.serenity

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.app.AppRuntimeRenderLoops
import com.serenity.config.AppConfig
import com.serenity.diagnostics.FrameTimings
import com.serenity.input.{InputRouter, PendingInput, SystemClipboard}
import com.serenity.keystroke.events.{Copy, Cut, Event, InsertChar, RunCommand}
import com.serenity.keystroke.translators.TextEntryTranslator
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.manager.StateManagerTestFacade.*
import com.serenity.state.models.Damage
import com.serenity.testkit.SharedDictionary
import fs2.{Chunk, Stream}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** The context menu runs Copy and Cut as commands (`RunCommand`), which enqueue the clipboard event inside the
  * dispatch. The text they copy has to reach the system clipboard the way a Ctrl+C does (#2511).
  */
class MenuClipboardExportSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private val logger = LoggerFactory[IO].getLogger(using LoggerName("MenuClipboardExportSpec"))

  private def typed(text: String): List[PendingInput] =
    text.toList.map(char => PendingInput.Ready(InsertChar(char)))

  private def inEditor(batches: List[List[PendingInput]]): IO[(Vector[String], String)] =
    for
      stateManager    <- StateManager(logger, dictionaryCache = SharedDictionary.default)
      bufferId        <- stateManager.createNewEmptyBuffer
      state           <- stateManager.getCurrentState
      _               <- stateManager.setBufferForPane(state.persisted.layout.editorPanes.keys.head, bufferId)
      written         <- Ref.of[IO, Vector[String]](Vector.empty)
      router          <- InputRouter.create[IO, Event](new TextEntryTranslator(AppConfig.default))
      cursorVisible   <- Ref.of[IO, Boolean](true)
      translatorCache <- Ref.of[IO, Option[AppRuntimeRenderLoops.FocusedTranslatorCacheEntry]](None)
      context = AppRuntimeRenderLoops.InputBatchContext(
        stateManager,
        router,
        SystemClipboard[IO](readText = IO.pure(None), writeText = text => written.update(_ :+ text)),
        IO.unit,
        cursorVisible,
        (_: Damage) => IO.unit,
        translatorCache,
        FrameTimings()
      )
      _ <- Stream
        .emits(batches.map(Chunk.from))
        .through(AppRuntimeRenderLoops.inputBatchPhase(context))
        .compile
        .drain
      writes <- written.get
      after  <- stateManager.getCurrentState
      content = after.persisted.buffers(bufferId).document.content.collect()
    yield (writes, content)

  "A menu-dispatched Copy" should "write the copied text to the system clipboard" in {
    val (writes, _) = inEditor(List(typed("ab"), List(PendingInput.Ready(RunCommand("copy"))))).unsafeRunSync()

    writes shouldBe Vector("ab")
  }

  "A menu-dispatched Cut" should "write the cut text to the system clipboard and remove it" in {
    val (writes, content) = inEditor(List(typed("ab"), List(PendingInput.Ready(RunCommand("cut"))))).unsafeRunSync()

    (writes, content) shouldBe (Vector("ab"), "")
  }

  "Copying the same text again" should "write it again, as another application may have replaced it" in {
    val copy        = List(PendingInput.Ready(RunCommand("copy")))
    val (writes, _) = inEditor(List(typed("ab"), copy, copy)).unsafeRunSync()

    writes shouldBe Vector("ab", "ab")
  }

  "A keyboard Copy" should "write to the system clipboard exactly once" in {
    val (writes, _) = inEditor(List(typed("ab"), List(PendingInput.Ready(Copy)))).unsafeRunSync()

    writes shouldBe Vector("ab")
  }

  "A keyboard Cut" should "write to the system clipboard exactly once" in {
    val (writes, _) = inEditor(List(typed("ab"), List(PendingInput.Ready(Cut)))).unsafeRunSync()

    writes shouldBe Vector("ab")
  }
