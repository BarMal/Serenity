package com.serenity

import java.nio.file.{Files, Path}

import cats.effect.unsafe.implicits.global
import com.serenity.session.SessionState
import com.serenity.state.manager.StateManager
import com.serenity.state.models.AppState
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RecentFilesSpec extends AnyFlatSpec with Matchers with StateManagerTestSupport:

  trait RecentFilesFixture:
    val sm: StateManager = createStateManager("RecentFilesSpec")
    val initialBufferId  = sm.getCurrentState.unsafeRunSync().bufferOrder.head
    val tmpDir           = Files.createTempDirectory("recent-files-spec")

    def tmpFile(name: String): Path =
      tmpDir.resolve(name)

  behavior of "Recent files"

  it should "return an empty list initially" in new RecentFilesFixture:
    sm.getRecentFiles.unsafeRunSync() shouldBe Nil

  it should "track a file path after saveBufferAs" in new RecentFilesFixture:
    val path = tmpFile("hello.scala")
    try
      sm.saveBufferAs(initialBufferId, path.toString).unsafeRunSync()
      sm.getRecentFiles.unsafeRunSync() shouldBe List(path)
    finally
      Files.deleteIfExists(path)
      Files.deleteIfExists(tmpDir)

  it should "list most recently saved files first" in new RecentFilesFixture:
    val pathA = tmpFile("a.scala")
    val pathB = tmpFile("b.scala")
    try
      sm.saveBufferAs(initialBufferId, pathA.toString).unsafeRunSync()
      sm.saveBufferAs(initialBufferId, pathB.toString).unsafeRunSync()
      sm.getRecentFiles.unsafeRunSync() shouldBe List(pathB, pathA)
    finally
      Files.deleteIfExists(pathA)
      Files.deleteIfExists(pathB)
      Files.deleteIfExists(tmpDir)

  it should "deduplicate — saving the same path again moves it to the front" in new RecentFilesFixture:
    val pathA = tmpFile("a.scala")
    val pathB = tmpFile("b.scala")
    try
      sm.saveBufferAs(initialBufferId, pathA.toString).unsafeRunSync()
      sm.saveBufferAs(initialBufferId, pathB.toString).unsafeRunSync()
      sm.saveBufferAs(initialBufferId, pathA.toString).unsafeRunSync()
      sm.getRecentFiles.unsafeRunSync() shouldBe List(pathA, pathB)
    finally
      Files.deleteIfExists(pathA)
      Files.deleteIfExists(pathB)
      Files.deleteIfExists(tmpDir)

  it should "round-trip recentFiles through SessionState serialization" in:
    val paths   = List(Path.of("/workspace/foo.scala"), Path.of("/workspace/bar.scala"))
    val state   = AppState.initial.copy(recentFiles = paths)
    val session = SessionState.fromAppState(state)
    session.recentFiles shouldBe paths.map(_.toString)
    val restored = SessionState.toAppState(session, Theme.default)
    restored.recentFiles.map(_.toString) shouldBe paths.map(_.toString)

end RecentFilesSpec
