package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.input.FocusedInputTranslator
import com.serenity.keystroke.events.*
import com.serenity.keystroke.{InputKey, KeyStrokeInfo}
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.testkit.SharedDictionary
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** A modal raised while the command runner is open owns focus until it closes, then hands it back to the runner. */
class CommandRunnerModalFocusSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private def withRunnerOpen(): (StateManager, SurfaceId) =
    val logger = LoggerFactory[IO].getLogger(using LoggerName("CommandRunnerModalFocusSpec"))
    val sm     = StateManager.apply(logger, dictionaryCache = SharedDictionary.default).unsafeRunSync()
    sm.applyEvent(InsertChar('x')).unsafeRunSync()
    sm.applyEvent(ToggleCommandRunner).unsafeRunSync()
    val runnerId = sm.getCurrentState.unsafeRunSync().commandPickerSurface.map(_.id).getOrElse(fail("no runner"))
    (sm, runnerId)

  "A close confirmation raised over the open command runner" should "keep focus until answered" in {
    val (sm, runnerId) = withRunnerOpen()

    sm.applyEvent(Quit).unsafeRunSync()

    val prompted = sm.getCurrentState.unsafeRunSync()
    prompted.topModal shouldBe defined
    prompted.persisted.focus shouldBe Focus.Modal

    sm.applyEvent(Escape).unsafeRunSync()

    val answered = sm.getCurrentState.unsafeRunSync()
    answered.topModal shouldBe None
    answered.commandPickerSurface.map(_.id) shouldBe Some(runnerId)
    answered.persisted.focus shouldBe Focus.Surface(runnerId)
  }

  // #2577 P1c-2: the command picker is a modeless modal like the finder, and only one modeless modal is open at a
  // time (`ModalStateReducer.show`), so the finder replaces the picker rather than stacking over it as the old
  // floating palette allowed. Escape therefore closes to the editor, not back to the picker.
  "The Go to File finder opened over the command picker" should "replace it, take focus and keystrokes until dismissed" in {
    val (sm, _) = withRunnerOpen()

    sm.applyEvent(GoToFile).unsafeRunSync()

    val opened   = sm.getCurrentState.unsafeRunSync()
    val finderId = opened.modalSurface.map(_.id).getOrElse(fail("no finder"))
    opened.persisted.focus shouldBe Focus.Surface(finderId)
    FocusedInputTranslator.forState(opened).translate(KeyStrokeInfo(InputKey.Character, Some('a'), Set.empty)) shouldBe
      ModalInsertChar('a')

    sm.applyEvent(Escape).unsafeRunSync()

    val dismissed = sm.getCurrentState.unsafeRunSync()
    dismissed.modalSurface shouldBe None
    dismissed.commandPickerSurface shouldBe None
    dismissed.persisted.focus shouldBe Focus.EditorPane(PaneId(0))
  }
