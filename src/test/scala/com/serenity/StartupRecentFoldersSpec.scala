package com.serenity

import java.nio.file.{Files, Path}

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.app.AppStartup
import com.serenity.command.{CommandIntent, CommandRegistry, FileIntent}
import com.serenity.state.manager.StateManager
import com.serenity.state.manager.StateManagerTestFacade.*
import com.serenity.state.models.*
import com.serenity.testkit.SharedDictionary
import com.serenity.ui.layout.ViewportSize
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The start page lists the recent folders after the recent files, and leaves out any that have gone. */
class StartupRecentFoldersSpec extends AnyFlatSpec with Matchers with StateManagerTestSupport:

  private val root = TestTemp.directory("startup-recent-folders-spec")

  private def absolute(path: String): Path = Path.of(path).toAbsolutePath.normalize

  private def folderActions(page: StartupPage): List[StartupAction] =
    page.actions.filter(_.id.startsWith("recent-folder:"))

  "The start page" should "offer each recent folder, most recent first, as a path-carrying command" in {
    val (first, second) = (absolute("/work/book"), absolute("/work/notes"))
    val page            = AppStartup.createStartPage(sessionExists = false, recentFolders = List(first, second))

    folderActions(page).map(_.label) shouldBe List(first.toString, second.toString)
    folderActions(page).map(_.command.intent) shouldBe
      List(
        CommandIntent.File(FileIntent.OpenRecentFolder(first)),
        CommandIntent.File(FileIntent.OpenRecentFolder(second))
      )
    folderActions(page).map(_.detail) shouldBe List(Some("Recent folder"), Some("Recent folder"))
  }

  it should "list recent files before recent folders, after the fixed actions" in {
    val file   = absolute("/work/a.md")
    val folder = absolute("/work/book")
    val page = AppStartup.createStartPage(
      sessionExists = false,
      recentFiles = List(file),
      recentFolders = List(folder)
    )

    page.actions.map(_.id).takeRight(2) shouldBe List(s"recent:$file", s"recent-folder:$folder")
  }

  it should "show a folder once and no more than it shows files" in {
    val many = (1 to 12).map(i => absolute(s"/dir/folder$i")).toList
    val page = AppStartup.createStartPage(
      sessionExists = false,
      recentFolders = Path.of("/dir/folder1/../folder1") :: many
    )

    folderActions(page).map(_.label) shouldBe many.take(StartupPageContent.RecentFilesLimit).map(_.toString)
  }

  it should "offer no folder when none was opened" in {
    folderActions(AppStartup.createStartPage(sessionExists = false)) shouldBe Nil
  }

  private def launch(sessionRoot: Path, name: String): IO[(StateManager, AppState)] =
    for
      manager <- StateManager.apply(
        testLogger(s"StartupRecentFoldersSpec-$name"),
        sessionRootOverride = Some(sessionRoot),
        dictionaryCache = SharedDictionary.default
      )
      state <- AppStartup.initializeState(manager, manager.sessionStartupInfo, Theme.default, ViewportSize(80, 24))
    yield (manager, state)

  private def startPageOf(state: AppState): StartupPage =
    state.startPageSurface
      .map(_.content)
      .collect { case SurfaceContent.StartPage(page) => page }
      .getOrElse(fail("no start page"))

  it should "list a folder opened in the last session, and drop it once the folder is gone" in {
    val sessionRoot = Files.createDirectories(root.resolve("session"))
    val kept        = Files.createDirectories(root.resolve("kept"))
    val gone        = Files.createDirectories(root.resolve("gone"))

    val program = for
      (first, _) <- launch(sessionRoot, "first")
      _          <- first.fileOpener.openFolder(gone)
      _          <- first.fileOpener.openFolder(kept)
      _          <- first.runtimeLifecycle.awaitEffects
      save = CommandRegistry.default.findCommand("save-session").getOrElse(fail("no save-session"))
      _               <- first.executeCommand(save)
      (_, whileThere) <- launch(sessionRoot, "second")
      _               <- IO.blocking(Files.delete(gone))
      (_, afterGone)  <- launch(sessionRoot, "third")
    yield (
      folderActions(startPageOf(whileThere)).map(_.label),
      folderActions(startPageOf(afterGone)).map(_.label)
    )

    program.unsafeRunSync() shouldBe ((List(kept.toString, gone.toString), List(kept.toString)))
  }
end StartupRecentFoldersSpec
