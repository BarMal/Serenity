package com.serenity

import java.util.concurrent.atomic.AtomicInteger

import com.serenity.richtext.{RichTextDocument, RichTextParagraph, RichTextRun}
import com.serenity.rope.{Balance, Leaf, Rope}
import com.serenity.state.models.*
import com.serenity.ui.accessibility.{AccessibilityRole, AccessibilitySnapshot}
import com.serenity.ui.layout.ViewportSize
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A rich document's soft break is a rope placeholder (U+FFFC); the text assistive technology reads must show it as the
  * newline it stands for, without the snapshot reading the rope until somebody asks.
  */
class AccessibilitySoftBreakSpec extends AnyFlatSpec with Matchers:
  given Balance = Balance.default

  private val viewport = ViewportSize(100, 30)

  private val document = RichTextDocument(
    List(
      RichTextParagraph(List(RichTextRun("first"), RichTextRun.softBreak(), RichTextRun("line"))),
      RichTextParagraph.plain("second")
    )
  )

  final private class CountingLeaf(text: String, val collected: AtomicInteger) extends Leaf(text):

    override def collect(): String =
      val _ = collected.incrementAndGet()
      super.collect()

  private def stateShowing(buffer: Buffer): AppState =
    AppState.initial.copy(persisted =
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

  private def richBuffer(content: Rope): Buffer =
    Buffer(
      id = BufferId(1),
      document = Document(content = content),
      richText = RichTextState().withSyncedDocument(Some(document), contentVersion = 0L)
    )

  private def valueOf(buffer: Buffer): Option[String] =
    AccessibilitySnapshot
      .from(stateShowing(buffer), viewport)
      .nodes
      .find(_.role == AccessibilityRole.Document)
      .flatMap(_.value)
      .map(_.text)

  "A rich buffer's accessible text" should "show the soft break as a newline" in {
    valueOf(richBuffer(Rope(document.plainText))) shouldBe Some("first\nline\nsecond")
  }

  it should "not read the rope while projecting" in {
    val collected = new AtomicInteger(0)
    val snapshot =
      AccessibilitySnapshot.from(stateShowing(richBuffer(new CountingLeaf(document.plainText, collected))), viewport)

    snapshot.nodes.exists(_.role == AccessibilityRole.Document) shouldBe true
    collected.get shouldBe 0
  }

  "A plain buffer's accessible text" should "leave a literal object-replacement character alone" in {
    valueOf(Buffer(BufferId(1), Document(Rope("a￼b")))) shouldBe Some("a￼b")
  }
