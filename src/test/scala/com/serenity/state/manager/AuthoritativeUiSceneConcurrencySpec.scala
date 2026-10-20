package com.serenity.state.manager

import java.awt.Font
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{CountDownLatch, Executors, TimeUnit}

import scala.concurrent.duration.*
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.jdk.CollectionConverters.IterableHasAsScala

import com.serenity.config.AppConfig
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.{Layout, ViewportSize, WorkspaceNode, WorkspaceNodeId, WorkspaceTree}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The prepared scene is shared by the paint thread and the dispatcher's hit-testing: neither may wait on a build of a
  * different state, and one state is laid out once however many callers ask (#1915). Every handoff is a latch, so
  * nothing here depends on timing.
  */
class AuthoritativeUiSceneConcurrencySpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll:

  given Balance = Balance.default

  private val workers    = Executors.newFixedThreadPool(3)
  given ExecutionContext = ExecutionContext.fromExecutor(workers)

  override protected def afterAll(): Unit =
    workers.shutdownNow()
    ()

  private val paneId   = PaneId(0)
  private val bufferId = BufferId(1)
  private val viewport = ViewportSize(80, 24)
  private val code     = Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val prose    = Font(Font.SERIF, Font.PLAIN, 12)

  private def stateWith(text: String): AppState =
    val buffer = Buffer.fromString(bufferId, text)
    AppState.initial.copy(persisted =
      AppState.initial.persisted.copy(
        buffers = Map(bufferId -> buffer),
        bufferOrder = List(bufferId),
        layout = Layout(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, bufferId)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId("editor-0"), paneId)))
        ),
        focus = Focus.EditorPane(paneId),
        config = AppConfig.default
      )
    )

  final private class GatedBuilds(gatedState: AppState):
    val builds  = AtomicInteger(0)
    val entered = CountDownLatch(1)
    val release = CountDownLatch(1)

    def scene: AuthoritativeUiScene =
      AuthoritativeUiScene(decorateBuild =
        real =>
          (state, size, codeFont, textFont, metrics) =>
            builds.incrementAndGet()
            if state eq gatedState then
              entered.countDown()
              release.await()
            real(state, size, codeFont, textFont, metrics)
      )

  "AuthoritativeUiScene" should "lay a state out once when the renderer and a hit-test ask for it concurrently" in {
    val state = stateWith("alpha\nbeta")
    val gate  = GatedBuilds(state)
    val scene = gate.scene

    val render = Future(scene.forState(state, viewport, code, prose))
    gate.entered.await(10, TimeUnit.SECONDS) shouldBe true
    val hitTest = Future(scene.forState(state, viewport, code, prose))
    gate.release.countDown()

    val rendered = Await.result(render, 10.seconds)
    Await.result(hitTest, 10.seconds) should be theSameInstanceAs rendered
    gate.builds.get() shouldBe 1
  }

  it should "give hit-testing geometry built from its own fonts, not the ones the renderer painted with" in {
    val state     = stateWith("alpha\nbeta")
    val codeFonts = java.util.concurrent.ConcurrentLinkedQueue[Font]()
    val scene = AuthoritativeUiScene(decorateBuild =
      real =>
        (st, size, codeFont, textFont, metrics) =>
          codeFonts.add(codeFont)
          real(st, size, codeFont, textFont, metrics)
    )
    val displayFont = Font(Font.MONOSPACED, Font.PLAIN, 23)
    val previewCode = FontLoader.previewFontForRole(state.persisted.config.editorConfig.fontConfig, TypographyRole.Code)
    assume(previewCode.getSize2D != displayFont.getSize2D)

    val painted = scene.forState(state, viewport, displayFont, displayFont)
    val hitTest = scene.forState(state, viewport)

    hitTest should not be theSameInstanceAs(painted)
    codeFonts.asScala.toList.map(_.getSize2D) shouldBe List(displayFont.getSize2D, previewCode.getSize2D)
    scene.forState(state, viewport) should be theSameInstanceAs hitTest
  }

  it should "not block a hit-test on another state while the renderer holds a cold build" in {
    val painting = stateWith("alpha\nbeta")
    val clicked  = stateWith("gamma\ndelta")
    val gate     = GatedBuilds(painting)
    val scene    = gate.scene

    val render = Future(scene.forState(painting, viewport, code, prose))
    gate.entered.await(10, TimeUnit.SECONDS) shouldBe true
    val hitTest = Future(scene.forState(clicked, viewport))
    val result  = scala.util.Try(Await.result(hitTest, 10.seconds))
    gate.release.countDown()
    Await.result(render, 10.seconds)

    result.isSuccess shouldBe true
  }

  it should "rebuild after a failed build instead of caching the failure" in {
    val state    = stateWith("alpha")
    val attempts = AtomicInteger(0)
    val scene = AuthoritativeUiScene(decorateBuild =
      real =>
        (st, size, codeFont, textFont, metrics) =>
          if attempts.incrementAndGet() == 1 then sys.error("layout failed")
          else real(st, size, codeFont, textFont, metrics)
    )

    scala.util.Try(scene.forState(state, viewport, code, prose)).isFailure shouldBe true
    scala.util.Try(scene.forState(state, viewport, code, prose)).isSuccess shouldBe true
  }
