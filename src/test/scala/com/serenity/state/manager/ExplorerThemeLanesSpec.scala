package com.serenity.state.manager

import java.io.IOException
import java.nio.file.{Files, Path}

import scala.concurrent.duration.*

import cats.effect.{Deferred, IO, Ref}
import cats.syntax.all.*
import com.serenity.config.PreferredWindowSize
import com.serenity.io.{FileEntry, FileManager}
import com.serenity.keystroke.events.{Enter, InsertChar, SwitchTheme}
import com.serenity.rope.Balance
import com.serenity.session.SessionManager
import com.serenity.state.manager.StateManagerTestFacade.*
import com.serenity.state.models.*
import com.serenity.state.undo.UndoState
import com.serenity.testkit.AwaitCondition.awaitValue
import com.serenity.testkit.VirtualTime.runVirtual
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.layout.{DirEntry, DirectoryTreeData, PanelPosition, PanelTarget}
import com.serenity.ui.presets.UiPresetStore
import com.serenity.ui.theme.config.{AppThemeManager, ThemeConfig, ThemeCreatorState}
import com.serenity.ui.theme.{DefaultThemes, Theme}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.noop.NoOpLogger

/** Explorer directory listings and theme loads run on `EffectLanes` (#1697 Wave 3): the I/O never holds the dispatcher,
  * a newer request supersedes an older one on the same lane, and a result that is no longer current when it reaches the
  * dispatcher is dropped.
  */
class ExplorerThemeLanesSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private type Outcome[A] = Either[Throwable, A]

  /** I/O that parks each request until the spec settles it, so a spec decides when -- and in what order -- it lands. */
  final private class Gates[K, A](pending: Ref[IO, List[(K, Deferred[IO, Outcome[A]])]], delivered: Ref[IO, List[K]]):

    def await(key: K): IO[A] =
      Deferred[IO, Outcome[A]].flatMap(gate =>
        pending.update(_ :+ (key -> gate)) >> gate.get.rethrow.flatTap(_ => delivered.update(_ :+ key))
      )

    def requested(key: K): IO[Boolean]  = pending.get.map(_.exists(_._1 == key))
    def pendingCount(key: K): IO[Int]   = pending.get.map(_.count(_._1 == key))
    def deliveredCount(key: K): IO[Int] = delivered.get.map(_.count(_ == key))

    def settle(key: K, outcome: Outcome[A]): IO[Unit] =
      awaitValue(requested(key))(identity) >>
        pending.get.flatMap(_.collect { case (`key`, gate) => gate }.traverse_(_.complete(outcome).void))

  private object Gates:
    def apply[K, A]: IO[Gates[K, A]] =
      (Ref.of[IO, List[(K, Deferred[IO, Outcome[A]])]](Nil), Ref.of[IO, List[K]](Nil)).mapN(new Gates(_, _))

  final private class GatedFileManager(listings: Gates[Path, List[FileEntry]]) extends FileManager:
    override def listDirectory(directory: Path): IO[List[FileEntry]] = listings.await(directory)

  final private class GatedThemeManager(loads: Gates[String, Theme], writes: Option[Gates[String, Path]])
      extends AppThemeManager:
    override def loadTheme(themeName: String): IO[Theme] = loads.await(themeName)
    override def listAvailableThemes: IO[List[String]]   = IO.pure(Nil)
    override def writeUserTheme(config: ThemeConfig): IO[Path] =
      writes.fold(IO.raiseError(new IllegalStateException("no theme writes expected")))(_.await(config.name))

  private class ErrorRecordingLogger(ref: Ref[IO, List[String]]) extends Logger[IO]:
    def error(message: => String): IO[Unit]               = ref.update(_ :+ message)
    def error(t: Throwable)(message: => String): IO[Unit] = ref.update(_ :+ s"$message: ${t.getMessage}")
    def warn(message: => String): IO[Unit]                = IO.unit
    def warn(t: Throwable)(message: => String): IO[Unit]  = IO.unit
    def info(message: => String): IO[Unit]                = IO.unit
    def info(t: Throwable)(message: => String): IO[Unit]  = IO.unit
    def debug(message: => String): IO[Unit]               = IO.unit
    def debug(t: Throwable)(message: => String): IO[Unit] = IO.unit
    def trace(message: => String): IO[Unit]               = IO.unit
    def trace(t: Throwable)(message: => String): IO[Unit] = IO.unit

  private val sessionRoot: Path = Files.createTempDirectory("explorer-theme-lanes-spec")

  private def managerWith(
    listings: Gates[Path, List[FileEntry]],
    themes: Gates[String, Theme],
    logger: Logger[IO] = NoOpLogger.impl[IO],
    themeWrites: Option[Gates[String, Path]] = None
  ): IO[StateManager] =
    for
      modelRef            <- Ref.of[IO, Model](Model(AppState.initial, UndoState(), Map.empty))
      themeNamesRef       <- Ref.of[IO, List[String]](Nil)
      quitSignal          <- Deferred[IO, Unit]
      lspQueue            <- LspEffectQueue.create
      mouseTargetCacheRef <- Ref.of[IO, Option[MouseTargetCache]](None)
      runtime = StateManagerRuntime
        .create(
          modelRef = modelRef,
          themeNamesRef = themeNamesRef,
          quitSignal = quitSignal,
          logger = logger,
          policy = SessionManager.SessionPolicy(),
          sessionRootOverride = Some(sessionRoot),
          themeManager = new GatedThemeManager(themes, themeWrites),
          lspQueue = lspQueue,
          mouseTargetCacheRef = mouseTargetCacheRef,
          onFontConfigChanged = (_: FontConfig) => IO.unit,
          deviceTextScaleProvider = IO.pure(1.0),
          configPersistencePath = None,
          uiPresetStore = UiPresetStore(sessionRoot.resolve("ui-presets.json")),
          windowSizeProvider = IO.pure(None),
          onPreferredWindowSizeChanged = (_: PreferredWindowSize) => IO.unit,
          fileDialog = None
        )
        .copy(fileManager = new GatedFileManager(listings))
      manager <- StateManager.fromRuntime(runtime)
    yield manager

  private val root  = Path.of("/workspace/project")
  private val child = root.resolve("src")
  private val other = Path.of("/workspace/elsewhere")

  private def entry(path: Path, isDirectory: Boolean): FileEntry =
    FileEntry(path, path.getFileName.toString, isDirectory, None, 0L)

  private def explorerTree(state: AppState): Option[DirectoryTreeData] =
    state.pinnedSurfaces.collectFirst { case UiSurface(_, SurfaceContent.DirectoryTree(tree, _), _, _) => tree }

  /** An explorer rooted at [[root]] whose `src` row is selected and focused, ready for Enter to expand it. */
  private def explorerOnChild(manager: StateManager): IO[Unit] =
    manager.loadDirectoryTree(root, List("src/")) >>
      manager.selectFileInExplorer(child) >>
      manager.switchToPinnedPanel(PanelTarget.ByPosition(PanelPosition.Left))

  private def theme(name: String): Theme = DefaultThemes.default.copy(name = name)

  private def themeName(manager: StateManager): IO[String] = manager.getCurrentState.map(_.persisted.theme.name)

  "An explorer directory listing" should "not hold the dispatcher: an editor keystroke lands while it is pending" in {
    val program =
      for
        listings <- Gates[Path, List[FileEntry]]
        themes   <- Gates[String, Theme]
        manager  <- managerWith(listings, themes)
        _        <- explorerOnChild(manager)
        _        <- manager.applyEvent(Enter).timeout(5.seconds)
        _        <- awaitValue(listings.requested(child))(identity)
        _ <- manager.updateStateValidated(state =>
          state.copy(persisted =
            state.persisted.copy(focus =
              state.persisted.layout.activeEditorPaneId.fold(state.persisted.focus)(
                Focus.EditorPane.apply
              )
            )
          )
        )
        _      <- manager.applyEvent(InsertChar('x')).timeout(5.seconds)
        typed  <- manager.getCurrentState.map(_.persisted.buffers.values.map(_.document.content.toString).toList)
        loaded <- manager.getCurrentState.map(explorerTree(_).flatMap(_.entries.get(child)))
      yield (typed, loaded)

    runVirtual(program) shouldBe (List("x"), None)
  }

  it should "apply the listing once it arrives while the explorer still shows its directory" in {
    val nested = entry(child.resolve("Main.scala"), isDirectory = false)
    val program =
      for
        listings <- Gates[Path, List[FileEntry]]
        themes   <- Gates[String, Theme]
        manager  <- managerWith(listings, themes)
        _        <- explorerOnChild(manager)
        _        <- manager.applyEvent(Enter)
        _        <- listings.settle(child, Right(List(nested)))
        tree     <- awaitValue(manager.getCurrentState)(explorerTree(_).exists(_.entries.contains(child)))
      yield explorerTree(tree).map(t => (t.entries.get(child), t.expandedPaths.contains(child)))

    runVirtual(program) shouldBe Some((Some(List(DirEntry(nested.path, "Main.scala", isDirectory = false))), true))
  }

  it should "drop a listing for a directory the explorer navigated away from before it arrived" in {
    val program =
      for
        listings <- Gates[Path, List[FileEntry]]
        themes   <- Gates[String, Theme]
        manager  <- managerWith(listings, themes)
        _        <- explorerOnChild(manager)
        _        <- manager.applyEvent(Enter)
        _        <- awaitValue(listings.requested(child))(identity)
        _        <- manager.loadDirectoryTree(other, List("notes.txt"))
        _        <- listings.settle(child, Right(List(entry(child.resolve("Main.scala"), isDirectory = false))))
        _        <- IO.sleep(1.second)
        tree     <- manager.getCurrentState.map(explorerTree)
      yield tree.map(t => (t.rootPath, t.entries.keySet))

    runVirtual(program) shouldBe Some((other, Set(other)))
  }

  it should "report a failed listing with the same message as before" in {
    val program =
      for
        errors   <- Ref.of[IO, List[String]](Nil)
        listings <- Gates[Path, List[FileEntry]]
        themes   <- Gates[String, Theme]
        manager  <- managerWith(listings, themes, new ErrorRecordingLogger(errors))
        _        <- explorerOnChild(manager)
        _        <- manager.applyEvent(Enter)
        _        <- listings.settle(child, Left(new IOException("permission denied")))
        logged   <- awaitValue(errors.get)(_.nonEmpty)
        tree     <- manager.getCurrentState.map(explorerTree)
      yield (logged, tree.map(_.entries.contains(child)))

    runVirtual(program) shouldBe (List(s"[FILE] Failed to load directory $child: permission denied"), Some(false))
  }

  "Opening an explorer root" should "pin the panel at once and fill in its listing when it arrives" in {
    val readme = entry(root.resolve("README.md"), isDirectory = false)
    val program =
      for
        listings <- Gates[Path, List[FileEntry]]
        themes   <- Gates[String, Theme]
        manager  <- managerWith(listings, themes)
        // The executor returns only once the command's lane work has settled, so it runs alongside the listing gate.
        opening <- manager.commandExecutor
          .executeCommand(
            com.serenity.command.Command.typed(
              "open-root",
              "Opens a project root.",
              com.serenity.command.CommandIntent.View(com.serenity.command.ViewIntent.PinExplorerPanel),
              com.serenity.command.CommandCategory.View
            )
          )
          .start
        cwd    <- com.serenity.io.FileUtils.getCurrentDirectory
        _      <- awaitValue(listings.requested(cwd))(identity)
        pinned <- manager.getCurrentState.map(explorerTree)
        _      <- listings.settle(cwd, Right(List(readme)))
        _      <- opening.joinWithNever
        filled <- awaitValue(manager.getCurrentState)(explorerTree(_).exists(_.entries.contains(cwd)))
        selected = filled.pinnedSurfaces.collectFirst {
          case UiSurface(_, SurfaceContent.DirectoryTree(_, sel), _, _) =>
            sel
        }
      yield (pinned.map(t => (t.rootPath, t.entries)), explorerTree(filled).map(_.entries(cwd)), selected, cwd)

    val (pinned, listed, selected, cwd) = runVirtual(program)
    pinned shouldBe Some((cwd, Map.empty))
    listed shouldBe Some(List(DirEntry(readme.path, "README.md", isDirectory = false)))
    selected shouldBe Some(Some(readme.path))
  }

  "A theme switch" should "not hold the dispatcher while the theme loads" in {
    val program =
      for
        listings <- Gates[Path, List[FileEntry]]
        themes   <- Gates[String, Theme]
        manager  <- managerWith(listings, themes)
        _        <- manager.applyEvent(SwitchTheme("alpha")).timeout(5.seconds)
        _        <- awaitValue(themes.requested("alpha"))(identity)
        _        <- manager.applyEvent(InsertChar('x')).timeout(5.seconds)
        typed    <- manager.getCurrentState.map(_.persisted.buffers.values.map(_.document.content.toString).toList)
      yield typed

    runVirtual(program) shouldBe List("x")
  }

  it should "apply only the latest of two rapid switches, whichever load finishes first" in {
    val program =
      for
        listings   <- Gates[Path, List[FileEntry]]
        themes     <- Gates[String, Theme]
        manager    <- managerWith(listings, themes)
        initial    <- themeName(manager)
        _          <- manager.applyEvent(SwitchTheme("alpha"))
        _          <- awaitValue(themes.requested("alpha"))(identity)
        _          <- manager.applyEvent(SwitchTheme("beta"))
        _          <- themes.settle("alpha", Right(theme("alpha")))
        _          <- IO.sleep(1.second)
        afterAlpha <- themeName(manager)
        _          <- themes.settle("beta", Right(theme("beta")))
        afterBeta  <- awaitValue(themeName(manager))(_ == "beta")
      yield (initial, afterAlpha, afterBeta)

    runVirtual(program) shouldBe ("dark", "dark", "beta")
  }

  it should "fill two explorers showing the same directory, not let one listing supersede the other" in {
    val nested = entry(child.resolve("Main.scala"), isDirectory = false)
    def explorerAt(position: PanelPosition, id: String)(state: AppState): AppState =
      com.serenity.DockedPanelFixtures.dock(
        state,
        SurfaceId(id),
        SurfaceContent.DirectoryTree(
          DirectoryTreeData(root, entries = Map(root -> List(DirEntry(child, "src", isDirectory = true)))),
          Some(child)
        ),
        position,
        30
      )
    def listingIn(position: PanelPosition)(state: AppState): Option[List[DirEntry]] =
      state.pinnedSurfaces
        .find(surface =>
          state.persisted.layout.workspaceTree.flatMap(_.positionForSurface(surface.id)).contains(position)
        )
        .collect { case UiSurface(_, SurfaceContent.DirectoryTree(tree, _), _, _) => tree }
        .flatMap(_.entries.get(child))
    val program =
      for
        listings <- Gates[Path, List[FileEntry]]
        themes   <- Gates[String, Theme]
        manager  <- managerWith(listings, themes)
        _ <- manager.updateStateValidated(
          explorerAt(PanelPosition.Left, "left").andThen(explorerAt(PanelPosition.Right, "right"))
        )
        _ <- manager.switchToPinnedPanel(PanelTarget.ByPosition(PanelPosition.Left))
        _ <- manager.applyEvent(Enter)
        _ <- manager.switchToPinnedPanel(PanelTarget.ByPosition(PanelPosition.Right))
        _ <- manager.applyEvent(Enter)
        _ <- awaitValue(listings.pendingCount(child))(_ == 2)
        _ <- listings.settle(child, Right(List(nested)))
        filled <- awaitValue(manager.getCurrentState)(state =>
          listingIn(PanelPosition.Left)(state).nonEmpty && listingIn(PanelPosition.Right)(state).nonEmpty
        )
      yield (listingIn(PanelPosition.Left)(filled), listingIn(PanelPosition.Right)(filled))

    val expected = Some(List(DirEntry(nested.path, "Main.scala", isDirectory = false)))
    runVirtual(program) shouldBe (expected, expected)
  }

  "A theme save" should "still land when a forced quit arrives while it is being written" in {
    val saved   = root.resolve("my-theme.conf")
    val creator = ThemeCreatorState.fromTheme(DefaultThemes.default.copy(name = "my-theme")).selectPath("theme.name")
    val program =
      for
        listings <- Gates[Path, List[FileEntry]]
        themes   <- Gates[String, Theme]
        writes   <- Gates[String, Path]
        manager  <- managerWith(listings, themes, themeWrites = Some(writes))
        _ <- manager.updateStateValidated { state =>
          val surface = UiSurface(
            SurfaceId("theme-creator"),
            SurfaceContent.ThemeCreator(creator),
            SurfacePresentation.Floating(None, SurfacePlacement.BelowCursor)
          )
          state.copy(
            persisted = state.persisted.copy(focus = Focus.Surface(surface.id)),
            runtime = state.runtime.copy(uiSurfaces = state.runtime.uiSurfaces :+ surface)
          )
        }
        _        <- manager.applyEvent(Enter)
        _        <- awaitValue(writes.requested("my-theme"))(identity)
        quitting <- manager.runtimeLifecycle.forceQuit.start
        _        <- IO.sleep(1.second)
        _        <- writes.settle("my-theme", Right(saved))
        _        <- quitting.joinWithNever
        landed   <- writes.deliveredCount("my-theme")
      yield landed

    runVirtual(program) shouldBe 1
  }

  "A theme load result" should "be dropped once a newer theme has been requested" in {
    val requestedBeta =
      AppState.initial.copy(runtime = AppState.initial.runtime.copy(requestedThemeName = Some("beta")))

    EffectResult.applyIfCurrent(requestedBeta, EffectResult.ThemeLoaded("alpha", theme("alpha"))) shouldBe requestedBeta
    EffectResult
      .applyIfCurrent(requestedBeta, EffectResult.ThemeLoaded("beta", theme("beta")))
      .persisted
      .theme
      .name shouldBe "beta"
  }
