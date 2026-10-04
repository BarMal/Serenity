package com.serenity.state.manager

import java.nio.file.Files

import scala.concurrent.duration.*
import scala.util.Random

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import cats.syntax.all.*
import com.serenity.config.AppConfig
import com.serenity.keystroke.events.{DeleteBackward, Enter, Event, InsertChar, Redo, ResizeEvent, Undo}
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManagerTestFacade.{createBuffer, updateState}
import com.serenity.state.models.*
import com.serenity.ui.layout.{ViewportSize, WrappedLineCache}
import com.serenity.{setBufferForPane, setCursorPosition}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.log4cats.noop.{NoOpFactory, NoOpLogger}

/** A batch centres a run of typed keys once rather than per key (#1985), so it must leave the state per-key dispatch
  * would, slice by slice, and never let anyone read the state with the cursor off its centred row.
  */
class TypedRunSettlingSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = NoOpFactory[IO]

  private val words =
    "lorem ipsum dolor sit amet consectetur adipiscing elit sed do eiusmod tempor incididunt ut labore".split(" ")

  private def proseDocument: String =
    val random = new Random(1985)
    Vector
      .fill(120)(Vector.fill(40 + random.nextInt(80))(words(random.nextInt(words.length))).mkString(" ") + ".")
      .mkString("\n\n")

  private def editor(config: AppConfig): IO[(StateManager, BufferId)] =
    for
      stateManager <- StateManager(NoOpLogger[IO], sessionRootOverride = Some(Files.createTempDirectory("typed-run")))
      _            <- stateManager.updateState(state => state.copy(persisted = state.persisted.copy(config = config)))
      bufferId     <- stateManager.createBuffer(proseDocument, None)
      state        <- stateManager.getCurrentState
      paneId = state.persisted.layout.editorPanes.keys.head
      _ <- stateManager.setBufferForPane(paneId, bufferId)
      _ <- stateManager.applyEvent(ResizeEvent(ViewportSize(110, 36)))
      _ <- stateManager.setCursorPosition(paneId, 60, 30)
      _ <- stateManager.applyEvent(InsertChar('w'))
    yield (stateManager, bufferId)

  private val oneSlice: EventBatchSteps[Event] = EventBatchSteps((_, event) => event, _ => false, 1.hour)

  private def applySlice(stateManager: StateManager, events: List[Event]): IO[Unit] =
    stateManager.applyEventBatch(events, oneSlice).void

  private def shown(stateManager: StateManager, bufferId: BufferId): IO[Option[(String, EditingState, Viewport)]] =
    stateManager.getCurrentState.map(
      _.persisted.buffers
        .get(bufferId)
        .map(buffer => (buffer.document.content.toString, buffer.editing, buffer.viewport))
    )

  private def typed(count: Int, seed: Long): List[Event] =
    val random = new Random(seed)
    List.fill(count)(InsertChar(if random.nextInt(6) == 0 then ' ' else ('a' + random.nextInt(26)).toChar))

  /** Applies `slices` batched on one editor and key by key on another, comparing them after every slice. */
  private def matchesPerKey(config: AppConfig, slices: List[List[Event]], after: List[Event] = Nil): Unit =
    val program = for
      (batched, bufferId) <- editor(config)
      (perKey, _)         <- editor(config)
      slicesDiffer <- slices.zipWithIndex.traverse { (slice, index) =>
        for
          _        <- applySlice(batched, slice)
          _        <- slice.traverse_(perKey.applyEvent)
          expected <- shown(perKey, bufferId)
          actual   <- shown(batched, bufferId)
        yield Option.when(actual != expected)(s"slice $index (${slice.size} events)")
      }
      afterDiffer <- after.zipWithIndex.traverse { (event, index) =>
        for
          _        <- batched.applyEvent(event)
          _        <- perKey.applyEvent(event)
          expected <- shown(perKey, bufferId)
          actual   <- shown(batched, bufferId)
        yield Option.when(actual != expected)(s"$event #$index after the slices")
      }
    yield (slicesDiffer ++ afterDiffer).flatten
    program.unsafeRunSync() shouldBe empty

  private val wrapped     = AppConfig.default.withWordWrap(true)
  private val typewriter  = wrapped.withSurfaceConfig(wrapped.surfaceConfig.copy(typewriterScrollingEnabled = true))
  private val unwrapped   = AppConfig.default.withWordWrap(false)
  private val sliceShapes = List(1, 5, 17, 40, 3, 64, 2).zipWithIndex.map((size, index) => typed(size, index.toLong))

  "A batch of typed keys" should "leave every slice as applying its keys one at a time would" in
    List(wrapped, typewriter, unwrapped).foreach(matchesPerKey(_, sliceShapes))

  it should "stay correct when one slice types more than incremental re-wrap resumes from" in
    matchesPerKey(wrapped, List(typed(300, 7L), typed(130, 8L)))

  it should "end a typed run at any other event, so those events see the cursor centred" in {
    val mixed = List(
      typed(12, 1L) ++ List(Enter) ++ typed(30, 2L) ++ List(DeleteBackward, DeleteBackward) ++ typed(9, 3L),
      typed(25, 4L) ++ List(Enter, Enter) ++ typed(4, 5L)
    )
    matchesPerKey(wrapped, mixed)
  }

  it should "undo and redo to the same centred viewports as per-key typing" in {
    val mixed = List(
      typed(20, 11L) ++ List(Enter) ++ typed(45, 12L) ++ List(DeleteBackward) ++ typed(70, 13L),
      typed(33, 14L) ++ List(Enter) ++ typed(15, 15L)
    )
    matchesPerKey(wrapped, mixed, after = List.fill(6)(Undo) ++ List.fill(4)(Redo) ++ List(Undo))
  }

  it should "never let the model be read with the cursor off its centred row" in {
    val program = for
      (stateManager, bufferId) <- editor(wrapped)
      seen                     <- Ref.of[IO, Vector[AppState]](Vector.empty)
      sample = stateManager.getCurrentState.flatMap(state =>
        seen.update(states => if states.lastOption.exists(_ eq state) then states else states :+ state)
      )
      _      <- sample.foreverM.background.surround(applySlice(stateManager, typed(400, 21L)) >> IO.sleep(5.millis))
      states <- seen.get
    yield (states, bufferId)
    val (states, bufferId) = program.unsafeRunSync()
    val uncentred = states.zipWithIndex.collect {
      case (state, index) if state.persisted.buffers.get(bufferId).exists { buffer =>
            buffer.editing.cursorPositions.headOption.exists(cursor =>
              CursorViewport.adjustForCursor(buffer, state, cursor, WrappedLineCache.Uncached) != buffer.viewport
            )
          } =>
        index
    }
    withClue(s"${states.size} states read: ")(uncentred shouldBe empty)
  }

  it should "apply and centre a lone key in the dispatch that takes it, without waiting for more to arrive" in {
    val program = for
      (batched, bufferId) <- editor(wrapped)
      (perKey, _)         <- editor(wrapped)
      _                   <- perKey.applyEvent(InsertChar('q'))
      expected            <- shown(perKey, bufferId)
      batch               <- batched.applyEventBatch(List[Event](InsertChar('q')), oneSlice).timeout(30.seconds)
      actual              <- shown(batched, bufferId)
    yield (batch.applied, batch.remaining, actual, expected)
    val (applied, remaining, actual, expected) = program.unsafeRunSync()
    applied shouldBe List(InsertChar('q'))
    remaining shouldBe empty
    actual shouldBe expected
  }
