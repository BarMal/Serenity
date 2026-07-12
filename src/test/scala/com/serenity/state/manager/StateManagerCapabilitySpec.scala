package com.serenity.state.manager

import java.nio.file.Path

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}
import com.serenity.app.AppRuntime
import com.serenity.config.AppConfig
import com.serenity.input.{InputRouter, SystemClipboard}
import com.serenity.keystroke.events.{Event, Paste, ResizeEvent}
import com.serenity.keystroke.translators.TextEntryTranslator
import com.serenity.rope.Balance
import com.serenity.state.models.{AppState, BufferId}
import com.serenity.ui.layout.ViewportSize
import com.serenity.ui.renderer.RenderController
import fs2.Stream
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StateManagerCapabilitySpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "StateManager" should "compose focused façade capabilities" in {
    summon[StateManager <:< FocusManager]
    summon[StateManager <:< BufferManager]
    summon[StateManager <:< PaneManager]
    summon[StateManager <:< PeekManager]
    summon[StateManager <:< SessionService]
    summon[StateManager <:< PanelManager]
    summon[StateManager <:< ModalService]
    summon[StateManager <:< FileService]
    summon[StateManager <:< ScrollManager]
    succeed
  }

  "StateManagerFileFacade" should "delegate file operations without a StateManager" in {
    val stateRef = Ref.of[IO, AppState](AppState.initial).unsafeRunSync()
    val calls    = Ref.of[IO, List[String]](Nil).unsafeRunSync()
    val facade = new StateManagerFileFacade(
      stateRef,
      path => calls.update(_ :+ s"open:$path"),
      bufferId => calls.update(_ :+ s"save:$bufferId"),
      (bufferId, path) => calls.update(_ :+ s"save-as:$bufferId:$path"),
      bufferId => calls.update(_ :+ s"close:$bufferId")
    )
    val path = Path.of("notes.md")

    facade.openFile(path).unsafeRunSync()
    facade.saveBuffer(BufferId(0)).unsafeRunSync()
    facade.saveBufferAs(BufferId(0), path).unsafeRunSync()
    facade.forceCloseBuffer(BufferId(0)).unsafeRunSync()

    calls.get.unsafeRunSync() shouldBe List(
      s"open:$path",
      "save:BufferId(0)",
      s"save-as:BufferId(0):$path",
      "close:BufferId(0)"
    )
  }

  "RenderController" should "depend only on the event application capability" in {
    val applied = Ref.of[IO, List[ResizeEvent]](Nil).unsafeRunSync()
    val events = new EventApplier:
      def applyEvent(event: com.serenity.keystroke.events.Event): IO[Unit] =
        event match
          case resize: ResizeEvent => applied.update(_ :+ resize)
          case _                   => IO.unit

    RenderController.handleResize(Some(ViewportSize(120, 40)), events, IO.unit).unsafeRunSync()

    applied.get.unsafeRunSync() shouldBe List(ResizeEvent(ViewportSize(120, 40)))
  }

  "AppRuntime input phase" should "depend only on state read, update, and event application capabilities" in {
    val stateRef = Ref.of[IO, AppState](AppState.initial).unsafeRunSync()
    val applied  = Ref.of[IO, List[Event]](Nil).unsafeRunSync()
    val capabilities = new StateReader with StateUpdater with EventApplier:
      def getCurrentState: IO[AppState]                       = stateRef.get
      def updateState(update: AppState => AppState): IO[Unit] = stateRef.update(update)
      def applyEvent(event: Event): IO[Unit]                  = applied.update(_ :+ event)
    val router = InputRouter.create[IO, Event](new TextEntryTranslator(AppConfig.default)).unsafeRunSync()
    val clipboard = new SystemClipboard[IO]:
      def readText: IO[Option[String]]      = IO.pure(Some("pasted"))
      def writeText(text: String): IO[Unit] = IO.unit
    val cursorVisible = Ref.of[IO, Boolean](true).unsafeRunSync()
    val breathIndex   = Ref.of[IO, Int](0).unsafeRunSync()

    AppRuntime
      .inputEventPhase(capabilities, router, clipboard, IO.unit, cursorVisible, breathIndex, IO.unit)(
        Stream.emit(Paste)
      )
      .compile
      .drain
      .unsafeRunSync()

    stateRef.get.unsafeRunSync().clipboard.shouldBe(Some("pasted"))
    applied.get.unsafeRunSync().shouldBe(List(Paste))
  }
