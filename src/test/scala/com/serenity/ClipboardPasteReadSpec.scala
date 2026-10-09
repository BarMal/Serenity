package com.serenity

import scala.concurrent.duration.*

import cats.effect.std.Queue
import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO, Ref}
import com.serenity.app.AppRuntimeRenderLoops
import com.serenity.config.AppConfig
import com.serenity.diagnostics.FrameTimings
import com.serenity.input.{InputRouter, PendingInput, SystemClipboard}
import com.serenity.keystroke.events.{Event, Paste}
import com.serenity.keystroke.translators.TextEntryTranslator
import com.serenity.keystroke.{InputKey, KeyStrokeInfo}
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.manager.StateManagerTestFacade.*
import com.serenity.state.models.{AppState, Focus}
import com.serenity.testkit.SharedDictionary
import fs2.{Chunk, Stream}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** A paste waits on the system clipboard, which can be slow to answer (#1962). The wait must hold neither the state
  * dispatcher nor the order of the keys typed around the paste.
  */
class ClipboardPasteReadSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private val logger = LoggerFactory[IO].getLogger(using LoggerName("ClipboardPasteReadSpec"))

  private def editorWithEmptyBuffer: IO[StateManager] =
    for
      stateManager <- StateManager(logger, dictionaryCache = SharedDictionary.default)
      bufferId     <- stateManager.createNewEmptyBuffer
      state        <- stateManager.getCurrentState
      _            <- stateManager.setBufferForPane(state.persisted.layout.editorPanes.keys.head, bufferId)
    yield stateManager

  private def focusedText(state: AppState): Option[String] =
    state.persisted.focus match
      case Focus.EditorPane(paneId) =>
        state.persisted.layout.editorPanes
          .get(paneId)
          .flatMap(_.bufferId)
          .flatMap(state.persisted.buffers.get)
          .map(_.document.content.collect())
      case _ => None

  private def key(char: Char): PendingInput =
    PendingInput.Keystroke(KeyStrokeInfo(InputKey.Character, Some(char), Set.empty))

  private def inputLoop(
    stateManager: StateManager,
    clipboard: SystemClipboard[IO],
    inputs: Queue[IO, Option[Chunk[PendingInput]]]
  ): IO[Unit] =
    for
      router          <- InputRouter.create[IO, Event](new TextEntryTranslator(AppConfig.default))
      cursorVisible   <- Ref.of[IO, Boolean](true)
      translatorCache <- Ref.of[IO, Option[AppRuntimeRenderLoops.FocusedTranslatorCacheEntry]](None)
      context = AppRuntimeRenderLoops.InputBatchContext(
        stateManager,
        router,
        clipboard,
        IO.unit,
        cursorVisible,
        _ => IO.unit,
        translatorCache,
        FrameTimings()
      )
      _ <- Stream.fromQueueNoneTerminated(inputs).through(AppRuntimeRenderLoops.inputBatchPhase(context)).compile.drain
    yield ()

  "A paste waiting on the system clipboard" should
    "leave the dispatcher free and apply the keys typed meanwhile after the pasted text" in {
      val program = for
        stateManager <- editorWithEmptyBuffer
        readStarted  <- Deferred[IO, Unit]
        answer       <- Deferred[IO, Option[String]]
        clipboard = SystemClipboard[IO](readText = readStarted.complete(()) >> answer.get, writeText = _ => IO.unit)
        inputs <- Queue.unbounded[IO, Option[Chunk[PendingInput]]]
        loop   <- inputLoop(stateManager, clipboard, inputs).start
        _      <- inputs.offer(Some(Chunk(key('a'), PendingInput.Ready(Paste), key('b'))))
        _      <- readStarted.get.timeout(10.seconds)
        _ <- stateManager
          .updateStateValidated(state =>
            state.copy(runtime = state.runtime.copy(chapterGhostsVisible = !state.runtime.chapterGhostsVisible))
          )
          .timeout(10.seconds)
        _          <- inputs.offer(Some(Chunk(key('c'))))
        duringRead <- stateManager.getCurrentState.map(focusedText)
        _          <- answer.complete(Some("X"))
        _          <- inputs.offer(None)
        _          <- loop.joinWithNever.timeout(10.seconds)
        afterRead  <- stateManager.getCurrentState.map(focusedText)
      yield (duringRead, afterRead)

      program.unsafeRunSync() shouldBe (Some("a"), Some("aXbc"))
    }
