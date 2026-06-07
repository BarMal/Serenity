package com.serenity

import java.nio.file.{Files, Paths}

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

import com.serenity.keystroke.events.Enter
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.ui.layout.PanelPosition

class FileExplorerSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  trait ExplorerFixture:
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("Test"))

    val sm: StateManager = StateManager
      .apply(logger)(using Balance.default, LoggerFactory[IO])
      .unsafeRunSync()

  behavior of "File Explorer panel"

  // ── loadDirectoryTree ─────────────────────────────────────────────────────

  it should "pin a directory listing panel at the Left position" in new ExplorerFixture:
    sm.loadDirectoryTree("/repo", List("src", "build.sbt")).unsafeRunSync()

    val state = sm.getCurrentState.unsafeRunSync()
    state.pinnedSurfaces should have size 1
    state.pinnedSurfaces.head.presentation match
      case SurfacePresentation.Pinned(PanelPosition.Left, _) => succeed
      case other                                             => fail(s"Expected Pinned(Left, _), got $other")

  it should "populate the listing with the provided file names" in new ExplorerFixture:
    sm.loadDirectoryTree("/repo", List("src", "test", "build.sbt")).unsafeRunSync()

    val state   = sm.getCurrentState.unsafeRunSync()
    val content = state.pinnedSurfaces.head.content
    content match
      case SurfaceContent.DirectoryTree(tree, _) =>
        tree.rootPath shouldBe Paths.get("/repo")
        tree.entries(Paths.get("/repo")).map(_.name) shouldBe List("src", "test", "build.sbt")
      case other => fail(s"Expected DirectoryTree, got $other")

  it should "mark entries ending with '/' as directories" in new ExplorerFixture:
    sm.loadDirectoryTree("/repo", List("src/", "build.sbt")).unsafeRunSync()

    val state = sm.getCurrentState.unsafeRunSync()
    val entries = state.pinnedSurfaces.head.content match
      case SurfaceContent.DirectoryTree(tree, _) => tree.entries(Paths.get("/repo"))
      case other                                 => fail(s"Expected DirectoryTree, got $other")

    entries.find(_.name == "src/").map(_.isDirectory) shouldBe Some(true)
    entries.find(_.name == "build.sbt").map(_.isDirectory) shouldBe Some(false)

  it should "replace an existing Left panel when called again" in new ExplorerFixture:
    sm.loadDirectoryTree("/old", List("a.txt")).unsafeRunSync()
    sm.loadDirectoryTree("/new", List("b.txt", "c.txt")).unsafeRunSync()

    val state = sm.getCurrentState.unsafeRunSync()
    state.pinnedSurfaces should have size 1
    state.pinnedSurfaces.head.content match
      case SurfaceContent.DirectoryTree(tree, _) =>
        tree.rootPath shouldBe Paths.get("/new")
        tree.entries(Paths.get("/new")).map(_.name) shouldBe List("b.txt", "c.txt")
      case other => fail(s"Expected DirectoryTree, got $other")

  // ── selectFileInExplorer ──────────────────────────────────────────────────

  it should "set selectedPath in the directory listing to the given file" in new ExplorerFixture:
    sm.loadDirectoryTree("/repo", List("src", "build.sbt")).unsafeRunSync()
    sm.selectFileInExplorer("/repo/build.sbt").unsafeRunSync()

    val state = sm.getCurrentState.unsafeRunSync()
    state.pinnedSurfaces.head.content match
      case SurfaceContent.DirectoryTree(_, selectedPath) =>
        selectedPath shouldBe Some(Paths.get("/repo/build.sbt"))
      case other => fail(s"Expected DirectoryTree, got $other")

  it should "do nothing when no directory panel is pinned" in new ExplorerFixture:
    sm.selectFileInExplorer("/repo/build.sbt").unsafeRunSync()
    sm.getCurrentState.unsafeRunSync().pinnedSurfaces shouldBe Nil

  it should "navigate into a selected directory when activated from the pinned explorer" in new ExplorerFixture:
    val rootDir   = Files.createTempDirectory("explorer-root")
    val childDir  = Files.createDirectory(rootDir.resolve("child"))
    val childFile = Files.createFile(childDir.resolve("nested.txt"))
    try
      sm.loadDirectoryTree(rootDir.toString, List("child/")).unsafeRunSync()
      sm.selectFileInExplorer(childDir.toString).unsafeRunSync()
      sm.switchToPinnedPanel(PanelPosition.Left).unsafeRunSync()

      sm.applyEvent(Enter).unsafeRunSync()

      val state = sm.getCurrentState.unsafeRunSync()
      state.pinnedSurfaces.head.content match
        case SurfaceContent.DirectoryTree(tree, selectedPath) =>
          tree.rootPath shouldBe rootDir
          tree.expandedPaths should contain(childDir)
          tree.entries.getOrElse(childDir, Nil).map(_.name) should contain("nested.txt")
          selectedPath shouldBe Some(childDir)
        case other => fail(s"Expected DirectoryTree, got $other")
    finally
      Files.deleteIfExists(childFile)
      Files.deleteIfExists(childDir)
      Files.deleteIfExists(rootDir)

  // ── dragFileToDirectory ───────────────────────────────────────────────────

  it should "move the source file into the target directory on the filesystem" in new ExplorerFixture:
    val srcDir  = Files.createTempDirectory("drag-src")
    val dstDir  = Files.createTempDirectory("drag-dst")
    val srcFile = Files.createFile(srcDir.resolve("hello.txt"))
    Files.writeString(srcFile, "content")
    try
      sm.dragFileToDirectory(srcFile.toString, dstDir.toString).unsafeRunSync()

      Files.exists(srcFile) shouldBe false
      Files.exists(dstDir.resolve("hello.txt")) shouldBe true
      new String(Files.readAllBytes(dstDir.resolve("hello.txt"))) shouldBe "content"
    finally
      Files.deleteIfExists(dstDir.resolve("hello.txt"))
      Files.deleteIfExists(srcFile)
      Files.deleteIfExists(srcDir)
      Files.deleteIfExists(dstDir)

  it should "remove the moved file from the source directory listing panel" in new ExplorerFixture:
    val srcDir  = Files.createTempDirectory("drag-src2")
    val dstDir  = Files.createTempDirectory("drag-dst2")
    val srcFile = Files.createFile(srcDir.resolve("mover.txt"))
    try
      sm.loadDirectoryTree(srcDir.toString, List("mover.txt", "keeper.txt")).unsafeRunSync()
      sm.dragFileToDirectory(srcFile.toString, dstDir.toString).unsafeRunSync()

      val state = sm.getCurrentState.unsafeRunSync()
      state.pinnedSurfaces.head.content match
        case SurfaceContent.DirectoryTree(tree, _) =>
          tree.entries(srcDir).map(_.name) should not contain "mover.txt"
          tree.entries(srcDir).map(_.name) should contain("keeper.txt")
        case other => fail(s"Expected DirectoryTree, got $other")
    finally
      Files.deleteIfExists(dstDir.resolve("mover.txt"))
      Files.deleteIfExists(srcFile)
      Files.deleteIfExists(srcDir)
      Files.deleteIfExists(dstDir)
