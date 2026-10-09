package com.serenity

import java.nio.file.{Files, Path}

import cats.effect.unsafe.implicits.global
import com.serenity.state.models.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A folder named on the command line takes the route the Open Folder form's confirm does. */
class LaunchFolderOpenSpec extends AnyFlatSpec with Matchers with StateManagerTestSupport:

  private def explorerRoots(state: AppState): List[Path] =
    state.pinnedSurfaces.map(_.content).collect { case SurfaceContent.DirectoryTree(tree, _, _) => tree.rootPath }

  "StateManager.fileOpener.openFolder" should "pin the Explorer on the folder and leave the start page" in {
    val folder = TestTemp.directory("launch-folder-open")
    try
      val state = (for
        stateManager <- createStateManagerIO("LaunchFolderOpenSpec")
        _            <- stateManager.fileOpener.openFolder(folder)
        _            <- stateManager.runtimeLifecycle.awaitEffects
        state        <- stateManager.getCurrentState
      yield state).unsafeRunSync()

      explorerRoots(state) shouldBe List(folder)
    finally Files.deleteIfExists(folder)
  }
