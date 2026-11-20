package com.serenity

import com.serenity.app.AppStartup
import com.serenity.command.CommandRegistry
import com.serenity.config.{AppConfig, AppMode}
import com.serenity.keystroke.events.*
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.state.reducers.{AppEffect, AppEventReducer}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A key that does nothing says why, in a notice that never takes focus, instead of leaving the user guessing. */
class KeyDidNothingNoticeSpec extends AnyFlatSpec with Matchers:
  given Balance = Balance.default

  private val registry = CommandRegistry.default

  private def editorIn(mode: AppMode): AppState = AppState.initial(AppConfig.default.withAppMode(mode))

  private def startPage: AppState =
    val base      = AppState.initial
    val surfaceId = SurfaceId("startup")
    val page      = AppStartup.createStartPage(sessionExists = false, recentFiles = Nil)
    base.copy(runtime =
      base.runtime.copy(uiSurfaces =
        List(
          UiSurface(
            surfaceId,
            SurfaceContent.StartPage(page),
            SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
          )
        )
      )
    )

  private def notices(result: com.serenity.state.reducers.ReducerResult): List[Notice] =
    result.effects.collect { case AppEffect.ShowNotice(notice) => notice }

  "A bound key whose command is hidden by the mode" should "show a notice naming the command and the mode it needs" in {
    val result = AppEventReducer.reduce(RunCommand("cut-to-darlings"), editorIn(AppMode.Code), registry)

    result.effects.collect { case AppEffect.ExecuteCommand(_) => 1 } shouldBe Nil
    val shown = notices(result)
    shown should have size 1
    shown.head.level shouldBe NoticeLevel.Info
    shown.head.message should (include("Cut to Darlings") and include("prose mode"))
  }

  "A bound key whose command is disabled" should "show a notice with the reason" in {
    val result = AppEventReducer.reduce(RunCommand("cut-to-darlings"), editorIn(AppMode.Prose), registry)

    notices(result).map(_.message).mkString should include("Needs a text selection.")
  }

  "A bound key naming no command" should "show a notice naming the id" in {
    val result = AppEventReducer.reduce(RunCommand("no-such-command"), editorIn(AppMode.Prose), registry)

    notices(result).map(_.message).mkString should include("no-such-command")
  }

  "A bound key whose command can run" should "run it and show no notice" in {
    val result = AppEventReducer.reduce(RunCommand("copy"), editorIn(AppMode.Prose), registry)

    notices(result) shouldBe Nil
    result.effects.collect { case AppEffect.ExecuteCommand(command) => command.name } shouldBe List("copy")
  }

  "The same dead key pressed twice" should "replace its notice rather than stack another" in {
    val result = AppEventReducer.reduce(RunCommand("cut-to-darlings"), editorIn(AppMode.Code), registry)

    notices(result).flatMap(_.topic) shouldBe List(NoticeTopic.KeyDidNothing)
  }

  "The start page's toggle keys" should "each say the start page has no such surface" in {
    val events = List(
      ToggleCommandRunner,
      ToggleContextualToolbar,
      ToggleShortcutsHelp,
      ToggleTabList,
      ToggleRecentFilesInMode
    )

    events.foreach { event =>
      val state  = startPage
      val result = AppEventReducer.reduce(event, state, registry)

      result.state shouldBe state
      val shown = notices(result)
      withClue(s"$event: ") { shown should have size 1 }
      shown.head.message should include("start page")
    }
  }
