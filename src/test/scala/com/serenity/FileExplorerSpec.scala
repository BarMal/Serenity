package com.serenity

import java.nio.file.Paths

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.rope.Balance
import com.serenity.state.manager.StateManager
import com.serenity.state.models.*
import com.serenity.ui.layout.PanelPosition
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class FileExplorerSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  trait ExplorerFixture:
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger = LoggerFactory[IO].getLogger(using LoggerName("Test"))
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
      case other => fail(s"Expected Pinned(Left, _), got $other")

  it should "populate the listing with the provided file names" in new ExplorerFixture:
    sm.loadDirectoryTree("/repo", List("src", "test", "build.sbt")).unsafeRunSync()

    val state = sm.getCurrentState.unsafeRunSync()
    val content = state.pinnedSurfaces.head.content
    content match
      case SurfaceContent.DirectoryListing(root, entries, _) =>
        root shouldBe Paths.get("/repo")
        entries.map(_.name) shouldBe List("src", "test", "build.sbt")
      case other => fail(s"Expected DirectoryListing, got $other")

  it should "mark entries ending with '/' as directories" in new ExplorerFixture:
    sm.loadDirectoryTree("/repo", List("src/", "build.sbt")).unsafeRunSync()

    val state = sm.getCurrentState.unsafeRunSync()
    val entries = state.pinnedSurfaces.head.content match
      case SurfaceContent.DirectoryListing(_, es, _) => es
      case other => fail(s"Expected DirectoryListing, got $other")

    entries.find(_.name == "src/").map(_.isDirectory) shouldBe Some(true)
    entries.find(_.name == "build.sbt").map(_.isDirectory) shouldBe Some(false)

  it should "replace an existing Left panel when called again" in new ExplorerFixture:
    sm.loadDirectoryTree("/old", List("a.txt")).unsafeRunSync()
    sm.loadDirectoryTree("/new", List("b.txt", "c.txt")).unsafeRunSync()

    val state = sm.getCurrentState.unsafeRunSync()
    state.pinnedSurfaces should have size 1
    state.pinnedSurfaces.head.content match
      case SurfaceContent.DirectoryListing(root, entries, _) =>
        root shouldBe Paths.get("/new")
        entries.map(_.name) shouldBe List("b.txt", "c.txt")
      case other => fail(s"Expected DirectoryListing, got $other")

  // ── selectFileInExplorer ──────────────────────────────────────────────────

  it should "set selectedPath in the directory listing to the given file" in new ExplorerFixture:
    sm.loadDirectoryTree("/repo", List("src", "build.sbt")).unsafeRunSync()
    sm.selectFileInExplorer("/repo/build.sbt").unsafeRunSync()

    val state = sm.getCurrentState.unsafeRunSync()
    state.pinnedSurfaces.head.content match
      case SurfaceContent.DirectoryListing(_, _, selectedPath) =>
        selectedPath shouldBe Some(Paths.get("/repo/build.sbt"))
      case other => fail(s"Expected DirectoryListing, got $other")

  it should "do nothing when no directory panel is pinned" in new ExplorerFixture:
    sm.selectFileInExplorer("/repo/build.sbt").unsafeRunSync()
    sm.getCurrentState.unsafeRunSync().pinnedSurfaces shouldBe Nil
