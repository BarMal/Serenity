package com.serenity.perf

import com.serenity.state.core.EditorState
import com.serenity.state.models.{AppStateValidation, BufferId}
import com.serenity.ui.layout.{LayoutEngine, PaneScroll}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MultiBufferFixtureSpec extends AnyFlatSpec with Matchers:

  import MultiBufferFixture.Kind

  private def linesOf(text: String): Set[String] = text.linesIterator.filter(_.nonEmpty).toSet

  "MultiBufferFixture.document" should "give each buffer index its own lines, so no cache keyed by text is shared" in
    Kind.values.foreach { kind =>
      withClue(kind.label) {
        val documents =
          (1 to 3).map(index => linesOf(MultiBufferFixture.document(kind, index, MultiBufferFixture.TestSize)))
        documents.foreach(_.size should be > 10)
        documents.combinations(2).foreach(pair => (pair.head intersect pair(1)) shouldBe empty)
      }
    }

  it should "be the same document for the same index, so a run can be repeated" in
    Kind.values.foreach { kind =>
      MultiBufferFixture.document(kind, 4, MultiBufferFixture.TestSize) shouldBe
        MultiBufferFixture.document(kind, 4, MultiBufferFixture.TestSize)
    }

  it should "make code of short lines and prose of long ones, each buffer about as many characters as the other kind" in {
    val code  = MultiBufferFixture.document(Kind.Code, 1, MultiBufferFixture.FullSize)
    val prose = MultiBufferFixture.document(Kind.Prose, 1, MultiBufferFixture.FullSize)

    code.linesIterator.map(_.length).max should be < 120
    prose.linesIterator.filter(_.nonEmpty).map(_.length).min should be > 300
    (prose.length.toDouble / code.length) should (be > 0.8 and be < 1.25)
  }

  "MultiBufferFixture.state" should "open the asked-for buffers, all different, with the first shown" in
    Kind.values.foreach { kind =>
      val state = MultiBufferFixture.state(kind, buffers = 20, MultiBufferFixture.TestSize)

      state.persisted.bufferOrder shouldBe (1 to 20).map(BufferId(_)).toList
      state.persisted.buffers.values.map(_.document.content.toString).toSet.size shouldBe 20
      state.focusedBufferId shouldBe Some(BufferId(1))
      state.persisted.layout.editorPanes should have size 1
      AppStateValidation.validationErrors(state) shouldBe Nil
    }

  it should "wrap prose, as the laptop prose rows do, and leave code unwrapped" in {
    MultiBufferFixture
      .state(Kind.Prose, 2, MultiBufferFixture.TestSize)
      .persisted
      .config
      .surfaceConfig
      .wordWrapEnabled shouldBe true
    MultiBufferFixture
      .state(Kind.Code, 2, MultiBufferFixture.TestSize)
      .persisted
      .config
      .surfaceConfig
      .wordWrapEnabled shouldBe false
  }

  it should "make a late-count buffer of prose with just under the exact limit of lines" in {
    val lines = MultiBufferFixture.document(Kind.Prose, 1, MultiBufferFixture.LateCountSize).linesIterator.size

    lines should be <= PaneScroll.ExactLineLimit
    lines should be > PaneScroll.ExactLineLimit - 4
  }

  "MultiBufferFixture.split" should "show a different buffer in each of the panes" in
    Seq(2, 4).foreach { panes =>
      val state = MultiBufferFixture.split(
        MultiBufferFixture.state(Kind.Code, buffers = 20, MultiBufferFixture.TestSize),
        panes
      )

      state.persisted.layout.orderedPaneIds should have size panes
      state.persisted.layout.editorPanes.values.flatMap(_.bufferId).toSet should have size panes
      AppStateValidation.validationErrors(state) shouldBe Nil
    }

  it should "lay two panes side by side and four as two columns of two" in {
    def rects(panes: Int) =
      val split  = MultiBufferFixture.split(MultiBufferFixture.state(Kind.Prose, 4, MultiBufferFixture.TestSize), panes)
      val layout = LayoutEngine.calculateLayout(split, LaptopFrameBenchmarks.viewport)
      LayoutEngine.calculatePaneLayouts(split, layout).values.toList

    rects(2).map(_.x).distinct should have size 2
    rects(2).map(_.y).distinct should have size 1
    rects(4).map(_.x).distinct should have size 2
    rects(4).map(_.y).distinct should have size 2
  }

  it should "leave a one-pane state as it is" in {
    val state = MultiBufferFixture.state(Kind.Code, buffers = 3, MultiBufferFixture.TestSize)

    MultiBufferFixture.split(state, 1) shouldBe state
  }

  "MultiBufferFixture.switchTo" should "show the buffer in the active pane" in {
    val state = MultiBufferFixture.state(Kind.Prose, buffers = 5, MultiBufferFixture.TestSize)

    MultiBufferFixture.switchTo(state, BufferId(4)).focusedBufferId shouldBe Some(BufferId(4))
    EditorState.switchToBuffer(state, BufferId(2)).focusedBufferId shouldBe Some(BufferId(2))
  }
