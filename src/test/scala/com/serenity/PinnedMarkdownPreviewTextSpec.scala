package com.serenity

import java.util.concurrent.atomic.AtomicInteger

import com.serenity.markdown.MarkdownPreviewCache
import com.serenity.rope.{Balance, Leaf, Rope}
import com.serenity.state.models.*
import com.serenity.ui.accessibility.AccessibilitySnapshot
import com.serenity.ui.layout.{LayoutEngine, LayoutRect, PanelPosition, UiSceneSnapshot, ViewportSize}
import com.serenity.ui.renderer.PinnedPanelViewModel
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A docked markdown preview is resolved on every frame and for every layout contract; it must read the buffer's text
  * only when that text is new, not each time the same document is resolved again.
  */
class PinnedMarkdownPreviewTextSpec extends AnyFlatSpec with Matchers:
  given Balance = Balance.default

  private val bufferId  = BufferId(1)
  private val surfaceId = SurfaceId("preview")
  private val rect      = LayoutRect(0, 0, 60, 10)

  /** See `AccessibilityLazyTextSpec.CountingLeaf`. */
  final private class CountingLeaf(text: String, val collected: AtomicInteger) extends Leaf(text):

    override def collect(): String =
      val _ = collected.incrementAndGet()
      super.collect()

  private def previewState(content: Rope): AppState =
    val buffer = Buffer(bufferId, Document(content))
    val base = AppState.initial.copy(persisted =
      AppState.initial.persisted.copy(
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = AppState.initial.persisted.layout.copy(
          editorPanes = Map(PaneId(0) -> EditorPane.withBuffer(PaneId(0), buffer.id)),
          activeEditorPaneId = Some(PaneId(0))
        ),
        focus = Focus.EditorPane(PaneId(0))
      )
    )
    DockedPanelFixtures.dock(
      base,
      surfaceId,
      SurfaceContent.MarkdownPreview(bufferId, "notes.md"),
      PanelPosition.Right,
      40
    )

  private def surfaceOf(state: AppState): UiSurface =
    state.pinnedSurfaces.headOption.getOrElse(fail("expected the docked preview"))

  "PinnedPanelViewModel.resolve" should "read an unchanged document's text once across repeated resolves" in {
    val collected = new AtomicInteger(0)
    val state     = previewState(new CountingLeaf("# Heading\n\nSome *body* text", collected))
    val cache     = MarkdownPreviewCache()

    val first  = PinnedPanelViewModel.resolve(surfaceOf(state), rect, state, cache)
    val second = PinnedPanelViewModel.resolve(surfaceOf(state), rect, state, cache)

    first.rows.map(_.plainText) should contain("Heading")
    second.rows.map(_.plainText) shouldBe first.rows.map(_.plainText)
    collected.get shouldBe 1
  }

  it should "show the edited text after the document changes" in {
    val collected = new AtomicInteger(0)
    val cache     = MarkdownPreviewCache()
    val before    = previewState(new CountingLeaf("# Heading", collected))
    val after     = previewState(new CountingLeaf("# Heading!", collected))

    PinnedPanelViewModel.resolve(surfaceOf(before), rect, before, cache).rows.map(_.plainText) should contain("Heading")
    PinnedPanelViewModel.resolve(surfaceOf(after), rect, after, cache).rows.map(_.plainText) should contain("Heading!")
    collected.get shouldBe 2
  }

  it should "keep the preview rows for the panel's current height" in {
    val collected = new AtomicInteger(0)
    val state     = previewState(new CountingLeaf((1 to 40).map(line => s"line $line").mkString("\n\n"), collected))
    val cache     = MarkdownPreviewCache()

    val short = PinnedPanelViewModel.resolve(surfaceOf(state), LayoutRect(0, 0, 60, 6), state, cache)
    val tall  = PinnedPanelViewModel.resolve(surfaceOf(state), LayoutRect(0, 0, 60, 20), state, cache)

    tall.rows.size should be > short.rows.size
  }

  "UiSceneSnapshot" should "read an unchanged document's text once across repeated projections sharing a cache" in {
    val collected = new AtomicInteger(0)
    val state     = previewState(new CountingLeaf("# Heading\n\nSome body text", collected))
    val viewport  = ViewportSize(120, 40)
    val layout    = LayoutEngine.calculateLayoutWithUI(state, viewport)
    val cache     = MarkdownPreviewCache()

    UiSceneSnapshot.from(state, layout, viewport, cache)
    UiSceneSnapshot.from(state, layout, viewport, cache)

    collected.get shouldBe 1
  }

  "AccessibilitySnapshot" should "read an unchanged previewed document's text once across projections sharing a cache" in {
    val collected = new AtomicInteger(0)
    val state     = previewState(new CountingLeaf("# Heading\n\nSome body text", collected))
    val cache     = MarkdownPreviewCache()

    val first = AccessibilitySnapshot.from(state, ViewportSize(120, 40), None, cache)
    AccessibilitySnapshot.from(state, ViewportSize(120, 40), Some(first), cache)

    collected.get shouldBe 1
  }
