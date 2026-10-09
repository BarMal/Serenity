package com.serenity

import java.nio.file.{Files, Path}

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.app.AppRuntime
import com.serenity.command.{Command, CommandCategory, CommandIntent, CommandRegistry, FileIntent}
import com.serenity.io.FileDialog
import com.serenity.state.manager.StateManager
import com.serenity.state.manager.StateManagerTestFacade.*
import com.serenity.state.models.{AppState, Persisted, SurfaceContent}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Every route that opens a folder as the project root records it, as opening a file records the file. */
class RecentFoldersSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll with StateManagerTestSupport:

  private val root = TestTemp.directory("recent-folders-spec")

  override protected def afterAll(): Unit =
    try super.afterAll()
    finally deleteRecursively(root)

  private def deleteRecursively(directory: Path): Unit =
    val paths = Files.walk(directory)
    try paths.sorted(java.util.Comparator.reverseOrder[Path]()).forEach(path => Files.delete(path))
    finally paths.close()

  private def folder(name: String): Path = Files.createDirectories(root.resolve(name))

  private def explorerRoots(state: AppState): List[Path] =
    state.pinnedSurfaces.map(_.content).collect { case SurfaceContent.DirectoryTree(tree, _, _) => tree.rootPath }

  private def recorded(stateManager: StateManager, expected: List[Path]): List[Path] =
    awaitState(stateManager)(_.persisted.recentFolders == expected)
      .timeout(scala.concurrent.duration.DurationInt(20).seconds)
      .attempt
      .flatMap(_ => stateManager.getCurrentState)
      .map(_.persisted.recentFolders)
      .unsafeRunSync()

  private def choosing(chosen: Path): FileDialog =
    FileDialog(
      chooseOpenFile = _ => IO.pure(None),
      chooseSaveFile = (_, _) => IO.pure(None),
      chooseFolder = _ => IO.pure(Some(chosen))
    )

  private def command(intent: FileIntent): Command =
    Command.typed("recent-folders-spec", "Recent folders spec.", CommandIntent.File(intent), CommandCategory.File)

  "Opening a folder" should "record it when chosen with Open Folder" in {
    val chosen       = folder("open-folder")
    val stateManager = createStateManager("RecentFoldersSpec-open-folder", fileDialog = Some(choosing(chosen)))

    stateManager.executeCommand(command(FileIntent.OpenFolder)).unsafeRunSync()

    recorded(stateManager, List(chosen)) shouldBe List(chosen)
  }

  it should "record it when named on the command line" in {
    val named        = folder("command-line")
    val stateManager = createStateManager("RecentFoldersSpec-command-line")

    stateManager.fileOpener.openFolder(named).unsafeRunSync()

    recorded(stateManager, List(named)) shouldBe List(named)
  }

  it should "record it when a later launch forwards it" in {
    val forwarded    = folder("forwarded")
    val stateManager = createStateManager("RecentFoldersSpec-forwarded")

    AppRuntime
      .openForwarded(stateManager.fileOpener)(List(forwarded))(using testLogger("RecentFoldersSpec"))
      .unsafeRunSync()

    recorded(stateManager, List(forwarded)) shouldBe List(forwarded)
  }

  it should "list the most recent folder first, once, however its path is spelt" in {
    val first        = folder("first")
    val second       = folder("second")
    val respelt      = root.resolve("second").resolve("..").resolve("first")
    val stateManager = createStateManager("RecentFoldersSpec-order")

    stateManager.fileOpener.openFolder(first).unsafeRunSync()
    stateManager.fileOpener.openFolder(second).unsafeRunSync()
    stateManager.fileOpener.openFolder(respelt).unsafeRunSync()

    recorded(stateManager, List(first, second)) shouldBe List(first, second)
  }

  it should "not record a path that is not a folder" in {
    val missing      = root.resolve("missing")
    val file         = Files.createFile(root.resolve("a-file.md"))
    val stateManager = createStateManager("RecentFoldersSpec-not-a-folder")

    stateManager.fileOpener.openFolder(missing).unsafeRunSync()
    stateManager.fileOpener.openFolder(file).unsafeRunSync()

    stateManager.getCurrentState.unsafeRunSync().persisted.recentFolders shouldBe Nil
  }

  "Choosing a recent folder" should "open it as the project root and move it to the front" in {
    val older        = folder("older")
    val newer        = folder("newer")
    val stateManager = createStateManager("RecentFoldersSpec-choose")
    stateManager.fileOpener.openFolder(older).unsafeRunSync()
    stateManager.fileOpener.openFolder(newer).unsafeRunSync()

    stateManager.executeCommand(command(FileIntent.OpenRecentFolder(older))).unsafeRunSync()

    recorded(stateManager, List(older, newer)) shouldBe List(older, newer)
    explorerRoots(stateManager.getCurrentState.unsafeRunSync()) shouldBe List(older)
  }

  "Clear Recent" should "forget the recent folders along with the recent files" in {
    val remembered   = folder("remembered")
    val stateManager = createStateManager("RecentFoldersSpec-clear")
    stateManager.fileOpener.openFolder(remembered).unsafeRunSync()
    recorded(stateManager, List(remembered)) shouldBe List(remembered)

    val clear = CommandRegistry.withToggleUI.findCommand("clear-recent-files").getOrElse(fail("no clear command"))
    stateManager.executeCommand(clear).unsafeRunSync()

    stateManager.getCurrentState.unsafeRunSync().persisted.recentFolders shouldBe Nil
  }

  "Persisted.trackRecentFolder" should "put the folder first, drop its earlier entry and keep the rest in order" in {
    val (a, b, c) = (Path.of("/w/a"), Path.of("/w/b"), Path.of("/w/c"))

    Persisted.trackRecentFolder(List(a, b, c), b) shouldBe List(b, a, c)
  }

  it should "store the absolute, normalised path, so differently spelt paths are one entry" in {
    val tracked = Persisted.trackRecentFolder(List(Path.of("/w/a")), Path.of("/w/x/../a/."))

    tracked shouldBe List(Path.of("/w/a"))
    Persisted.trackRecentFolder(Nil, Path.of("relative/dir")) shouldBe
      List(Path.of("relative/dir").toAbsolutePath.normalize)
  }

  it should "keep the twenty most recent folders, as recent files do" in {
    val folders = (1 to 25).map(i => Path.of(s"/w/f$i")).toList
    val tracked = folders.foldLeft(List.empty[Path])(Persisted.trackRecentFolder)

    tracked shouldBe folders.reverse.take(20)
  }
