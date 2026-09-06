package com.serenity

import java.nio.file.Paths

import com.serenity.config.AppMode
import com.serenity.rope.Balance
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The "recent in this mode" list (issue #1307) reads off `Persisted.recentFilesByMode` for whichever mode is currently
  * active -- it never looks at another mode's history.
  */
class RecentFilesInModeSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  "RecentFilesInModeContent.build" should "list the current mode's recent files, most recent first" in {
    val codePaths  = List(Paths.get("/tmp/b.scala"), Paths.get("/tmp/a.scala"))
    val prosePaths = List(Paths.get("/tmp/notes.md"))
    val state = AppState.initial.copy(persisted =
      AppState.initial.persisted.copy(
        recentFilesByMode = Map(AppMode.Code -> codePaths, AppMode.Prose -> prosePaths)
      )
    )

    RecentFilesInModeContent.build(state) shouldBe SurfaceContent.RecentFilesInMode(AppMode.Code, codePaths)
  }

  it should "reflect the active mode, not always code" in {
    val prosePaths  = List(Paths.get("/tmp/notes.md"))
    val proseConfig = AppState.initial.persisted.config.withAppMode(AppMode.Prose)
    val state = AppState.initial.copy(persisted =
      AppState.initial.persisted.copy(
        config = proseConfig,
        recentFilesByMode = Map(AppMode.Prose -> prosePaths)
      )
    )

    RecentFilesInModeContent.build(state) shouldBe SurfaceContent.RecentFilesInMode(AppMode.Prose, prosePaths)
  }

  it should "return an empty list rather than fail when nothing has been opened in this mode yet" in {
    RecentFilesInModeContent.build(AppState.initial) shouldBe SurfaceContent.RecentFilesInMode(AppMode.Code, Nil)
  }
