package com.serenity.ui.tui

import scala.concurrent.duration.*

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import com.serenity.app.AppStartup
import com.serenity.config.AppConfig
import com.serenity.frontend.MarkdownPreviewWindowAvailability
import com.serenity.keystroke.events.{Enter, InsertChar}
import com.serenity.session.SessionManager
import com.serenity.testkit.AwaitCondition
import com.serenity.ui.layout.ViewportSize
import com.serenity.ui.theme.Theme
import com.serenity.ui.theme.config.AppThemeManager
import com.serenity.{StateManagerTestSupport, TestTemp}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The state manager a real TUI session runs on saves unsaved edits into the session once typing pauses (#1904). */
class TuiEditIdleSessionSaveSpec extends AnyFlatSpec with Matchers with StateManagerTestSupport:

  "TuiRuntime.makeStateManager" should "save unsaved edits into the session once typing pauses" in {
    val saved = (for
      sessionRoot <- IO.blocking(TestTemp.directory("tui-edit-idle-session-save"))
      makeEditor = TuiRuntime.makeStateManager(
        AppConfig.default,
        sessionRootOverride = Some(sessionRoot),
        configPersistencePath = None,
        previewWindowAvailability = MarkdownPreviewWindowAvailability.Unavailable
      )
      editor <- makeEditor(testLogger("TuiEditIdleSessionSaveSpec"))
      _      <- AppStartup.initializeState(editor, editor.sessionStartupInfo, Theme.default, ViewportSize(80, 24))
      _      <- editor.applyEvent(Enter)
      _      <- "draft".toList.traverse_(char => editor.applyEvent(InsertChar(char)))
      reader = SessionManager.create(
        sessionRoot,
        AppThemeManager.create,
        testLogger("TuiEditIdleSessionSaveSpec-reader"),
        SessionManager.SessionPolicy()
      )
      savedTexts = reader
        .loadSession()
        .map(_.toList.flatMap(_.persisted.buffers.values.map(_.document.content.toString)))
      texts <- AwaitCondition.awaitValue(savedTexts)(_.contains("draft"))
    yield texts).timeout(30.seconds).unsafeRunSync()

    saved should contain("draft")
  }
