package com.serenity

import com.serenity.app.AppStartup
import com.serenity.command.CommandRegistry
import com.serenity.config.{AppConfig, AppMode}
import com.serenity.keystroke.events.*
import com.serenity.project.ProjectPresence
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

  private def messageFor(event: GlobalAppEvent, state: AppState): String =
    val shown = notices(AppEventReducer.reduce(event, state, registry))
    withClue(s"$event: ")(shown should have size 1)
    shown.head.level shouldBe NoticeLevel.Info
    shown.head.message.length should be <= 56
    shown.head.message

  "A bound key whose command the mode hides" should "say which mode the command works in" in {
    messageFor(
      RunCommand("cut-to-darlings"),
      editorIn(AppMode.Code)
    ) shouldBe "Cut to Darlings works in prose mode only."
    messageFor(RunCommand("bold"), editorIn(AppMode.Code)) shouldBe "Bold works in prose mode only."
    messageFor(RunCommand("lsp-hover"), editorIn(AppMode.Prose)) shouldBe "LSP Hover works in code mode only."
  }

  it should "run nothing" in {
    val result = AppEventReducer.reduce(RunCommand("bold"), editorIn(AppMode.Code), registry)

    result.effects.collect { case AppEffect.ExecuteCommand(_) => 1 } shouldBe Nil
  }

  "A bound key whose command needs a selection" should "ask for one, action first" in {
    messageFor(RunCommand("cut-to-darlings"), editorIn(AppMode.Prose)) shouldBe "Select text to use Cut to Darlings."
  }

  "A bound key whose command needs a project" should "ask for a project folder" in {
    val noProject = editorIn(AppMode.Code)
      .copy(runtime = editorIn(AppMode.Code).runtime.copy(projectPresence = ProjectPresence.NotDetected))
    val label = registry.findCommand("project-build").map(_.label).getOrElse(fail("no project-build"))

    messageFor(RunCommand("project-build"), noProject) shouldBe s"Open a project folder to use $label."
  }

  "A bound key naming no command" should "name the id" in {
    messageFor(RunCommand("no-such-command"), editorIn(AppMode.Prose)) shouldBe "No command named \"no-such-command\"."
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

  "The start page's toggle keys" should "each say what to do first, in one short sentence" in {
    val expected = List(
      ToggleCommandRunner     -> "Open or create a document to use the command palette.",
      ToggleContextualToolbar -> "Open or create a document to use the toolbar.",
      ToggleShortcutsHelp     -> "Open or create a document to see shortcut help.",
      ToggleTabList           -> "Open or create a document to use the tab list.",
      ToggleRecentFilesInMode -> "Open or create a document to show recent files."
    )

    expected.foreach { (event, message) =>
      val state = startPage
      AppEventReducer.reduce(event, state, registry).state shouldBe state
      messageFor(event, state) shouldBe message
    }
  }
