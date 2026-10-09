package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.command.CommandRegistry
import com.serenity.config.{HotkeyAction, HotkeyConfig}
import com.serenity.keystroke.events.ToggleChapterGhosts
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.AppState
import com.serenity.state.reducers.AppEventReducer
import com.serenity.testkit.SharedDictionary
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** Ctrl+Shift+G shows or hides chapter ghosts. Ctrl+G stays Go to Line, so the ghost key is the shifted variant of it,
  * the same shift-for-the-related-action pattern as Ctrl+Shift+F over Ctrl+F.
  */
class ChapterGhostHotkeySpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "Show/Hide Chapter Ghosts" should "be bound to Ctrl+Shift+G (Cmd+Shift+G on macOS)" in
    List("Linux", "Windows", "Mac OS X").foreach { osName =>
      val modifier = if osName == "Mac OS X" then "meta" else "ctrl"
      val binding  = HotkeyConfig.defaultBindingsFor(osName)(HotkeyAction.ToggleChapterGhosts).map(_.render)

      binding shouldBe List(s"$modifier+shift+g")
    }

  it should "leave Ctrl+G as Go to Line and collide with nothing" in
    List("Linux", "Windows", "Mac OS X").foreach { osName =>
      val bindings = HotkeyConfig.defaultBindingsFor(osName)

      bindings(HotkeyAction.GoToLine).map(_.render) shouldBe List(if osName == "Mac OS X" then "meta+g" else "ctrl+g")
      HotkeyConfig.validate(bindings) shouldBe Right(())
    }

  it should "flip the ghost flag when its event is reduced" in {
    val reduced = AppEventReducer.reduce(ToggleChapterGhosts, AppState.initial, CommandRegistry.withToggleUI)

    reduced.state.runtime.chapterGhostsVisible shouldBe false
  }

  it should "flip the ghost flag through the whole event pipeline, and back" in {
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("Test"))
    val sm: StateManager    = StateManager.apply(logger, dictionaryCache = SharedDictionary.default).unsafeRunSync()

    sm.applyEvent(ToggleChapterGhosts).unsafeRunSync()
    sm.getCurrentState.unsafeRunSync().runtime.chapterGhostsVisible shouldBe false

    sm.applyEvent(ToggleChapterGhosts).unsafeRunSync()
    sm.getCurrentState.unsafeRunSync().runtime.chapterGhostsVisible shouldBe true
  }
