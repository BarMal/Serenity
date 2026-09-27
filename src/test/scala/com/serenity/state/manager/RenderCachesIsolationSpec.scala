package com.serenity.state.manager

import cats.effect.IO
import cats.effect.syntax.all.*
import cats.effect.unsafe.implicits.global
import com.serenity.TestWorkspaceTrees
import com.serenity.config.AppConfig
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.layout.{Layout, ViewportSize}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

/** #1677 acceptance criterion 1: "Two `StateManager`s with different cache capacities can run concurrently in one JVM."
  * Before this issue, [[RendererFrameState.cacheCapacity]], `ThemeManager`'s highlight/lex caches,
  * `CharacterRenderer.graphemeSegmentationCache`, and `AuthoritativeUiScene`'s prepared-scene cache (with its own
  * `synchronized` lock) were all JVM-wide singleton `object`s -- shared, with no notion of which `StateManager` a given
  * render or mouse-hit-testing call belonged to. Configuring one `StateManager`'s frame-state cache capacity, or even
  * just rendering through it, could silently retune or evict entries a completely independent `StateManager` was
  * relying on, and two such managers rendering at the same moment shared a single lock.
  *
  * This spec pins the fix directly: two independently constructed `StateManager`s, with different configured
  * [[RendererFrameState]] capacities and rendering distinct content, driven concurrently (interleaved on purpose, not
  * just sequentially) against a single JVM, must never let one manager's render caches answer for -- or be reconfigured
  * by -- the other's.
  */
class RenderCachesIsolationSpec extends AnyFlatSpec with Matchers:

  given Balance           = Balance.default
  given LoggerFactory[IO] = Slf4jFactory.create[IO]

  private val paneId = PaneId(0)

  private def stateWith(bufferId: BufferId, text: String): AppState =
    val buffer = Buffer.fromString(bufferId, text)
    val base   = AppState.initial
    base.copy(
      persisted = base.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(TestWorkspaceTrees.linear(paneId))
        )
      ),
      runtime = base.runtime.copy(viewportSize = Some(ViewportSize(80, 24)))
    )

  private def newStateManager(capacity: Int): IO[StateManager] =
    StateManager.apply(
      Slf4jFactory.create[IO].getLogger(using LoggerName("RenderCachesIsolationSpec")),
      initialConfig = AppConfig.default.withRendererFrameStateCacheCapacity(capacity)
    )

  "Two StateManagers with different render-frame-state cache capacities" should
    "keep each other's configured capacity independent, even when read and reconfigured in an interleaved order" in {
      val program =
        for
          managerA <- newStateManager(16)
          managerB <- newStateManager(128)
        yield
          // Neither construction perturbed the other's configured capacity -- a shared `AtomicInteger` would have
          // let whichever `StateManager` was built last win for both.
          managerA.renderCaches.frameState.currentCacheCapacity shouldBe 16
          managerB.renderCaches.frameState.currentCacheCapacity shouldBe 128

          // Reconfiguring one's capacity (the live path `StateManagerConfigEffects` drives on a config change) must
          // leave the other's completely untouched.
          managerA.renderCaches.frameState.configureCacheCapacity(64)
          managerA.renderCaches.frameState.currentCacheCapacity shouldBe 64
          managerB.renderCaches.frameState.currentCacheCapacity shouldBe 128

      program.unsafeRunSync()
    }

  it should "never let one manager's prepared scene or cache identity answer for the other's, under concurrent access" in {
    def renderAndCheck(manager: StateManager, bufferId: BufferId, tag: Int): IO[Unit] = IO {
      val state        = stateWith(bufferId, s"content-$tag\nline two\nline three")
      val viewportSize = ViewportSize(80, 24)
      val sceneA       = manager.renderCaches.authoritativeScene.forState(state, viewportSize)
      val sceneB       = manager.renderCaches.authoritativeScene.forState(state, viewportSize)
      // Same owner, same inputs: the prepared-scene cache on this instance must actually hit.
      sceneB shouldBe theSameInstanceAs(sceneA)
      sceneA.paneLayouts.keySet shouldBe Set(paneId)
    }

    val program =
      for
        managerA <- newStateManager(16)
        managerB <- newStateManager(128)
        // Interleaved, concurrent traffic against both managers' render caches at once -- the shape a shared JVM-wide
        // cache/lock (the pre-#1677 design) would have let cross-contaminate or serialize on.
        _ <- (0 until 40).toList.parTraverseN(16) { i =>
          if i % 2 == 0 then renderAndCheck(managerA, BufferId(i), i)
          else renderAndCheck(managerB, BufferId(i), i)
        }
      yield ()

    program.unsafeRunSync()
  }

  it should "hold distinct owner instances per StateManager, not a shared singleton" in {
    val program =
      for
        managerA <- newStateManager(16)
        managerB <- newStateManager(128)
      yield
        (managerA.renderCaches ne managerB.renderCaches) shouldBe true
        (managerA.renderCaches.frameState ne managerB.renderCaches.frameState) shouldBe true
        (managerA.renderCaches.themeHighlightCache ne managerB.renderCaches.themeHighlightCache) shouldBe true
        (managerA.renderCaches.graphemeSegmentationCache ne managerB.renderCaches.graphemeSegmentationCache) shouldBe true
        (managerA.renderCaches.authoritativeScene ne managerB.renderCaches.authoritativeScene) shouldBe true
        (managerA.renderCaches.markdownPreviewCache ne managerB.renderCaches.markdownPreviewCache) shouldBe true

    program.unsafeRunSync()
  }
