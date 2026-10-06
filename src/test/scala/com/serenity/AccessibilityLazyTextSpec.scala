package com.serenity

import java.util.concurrent.atomic.AtomicInteger
import javax.accessibility.AccessibleText
import javax.swing.JPanel
import javax.swing.text.JTextComponent

import scala.collection.mutable.ListBuffer

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.rope.{Balance, Leaf, Rope}
import com.serenity.state.models.*
import com.serenity.ui.accessibility.{
  AccessibilityPublishGate,
  AccessibilityRole,
  AccessibilitySnapshot,
  AccessibilitySync,
  SwingAccessibilityBridge,
  TuiAccessibilityBridge
}
import com.serenity.ui.layout.{CellMetrics, ViewportSize}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The accessibility projection must not read a document's text until somebody asks for it: projecting, memoizing,
  * gating and publishing a snapshot are all per-frame work, and a one-character edit must not make any of them
  * proportional to the document's size.
  */
class AccessibilityLazyTextSpec extends AnyFlatSpec with Matchers:
  given Balance = Balance.default

  private val viewport = ViewportSize(100, 30)
  private val metrics  = CellMetrics(8, 16, 12)

  /** A single-leaf rope that counts how often its text is materialised -- the same trick `RopeMetadataAndTraversalSpec`
    * uses, since `Rope` is sealed and `Leaf` is its one open subtype.
    */
  final private class CountingLeaf(text: String, val collected: AtomicInteger) extends Leaf(text):

    override def collect(): String =
      val _ = collected.incrementAndGet()
      super.collect()

  private def stateShowing(content: Rope): AppState =
    val buffer = Buffer(BufferId(1), Document(content))
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

  private def counted(text: String, collected: AtomicInteger): Rope = new CountingLeaf(text, collected)

  private def projection(state: AppState, previous: Option[AccessibilitySnapshot]): IO[AccessibilitySnapshot] =
    IO(AccessibilitySnapshot.from(state, viewport, previous))

  private def documentNode(snapshot: AccessibilitySnapshot) =
    snapshot.nodes.find(_.role == AccessibilityRole.Document)

  "AccessibilitySnapshot" should "not read the document text when projecting an edit" in {
    val collected = new AtomicInteger(0)
    val before    = AccessibilitySnapshot.from(stateShowing(counted("hello", collected)), viewport)
    val after     = AccessibilitySnapshot.from(stateShowing(counted("hello!", collected)), viewport, Some(before))

    documentNode(after).map(_.name) shouldBe Some("Untitled document")
    collected.get shouldBe 0
  }

  it should "still hand the text to whoever asks for it" in {
    val collected = new AtomicInteger(0)
    val snapshot  = AccessibilitySnapshot.from(stateShowing(counted("hello\nworld", collected)), viewport)

    documentNode(snapshot).flatMap(_.value).map(_.text) shouldBe Some("hello\nworld")
    collected.get shouldBe 1
  }

  "AccessibilitySync" should "not read the document text when an edit changes the state" in {
    val collected = new AtomicInteger(0)
    val stateA    = stateShowing(counted("hello", collected))
    val stateB    = stateShowing(counted("hello!", collected))
    val program = for
      sync   <- AccessibilitySync.empty
      first  <- sync.sync(stateA)(projection(stateA, _))
      second <- sync.sync(stateB)(projection(stateB, _))
    yield (first, second)

    val (first, second) = program.unsafeRunSync()
    (second eq first) shouldBe false
    collected.get shouldBe 0
  }

  "AccessibilityPublishGate" should "admit an edited document and refuse an unchanged one without reading text" in {
    val collected = new AtomicInteger(0)
    val rope      = counted("hello", collected)
    val gate      = new AccessibilityPublishGate
    val first     = AccessibilitySnapshot.from(stateShowing(rope), viewport)
    val rebuilt   = AccessibilitySnapshot.from(stateShowing(rope), viewport)
    val edited    = AccessibilitySnapshot.from(stateShowing(counted("hello!", collected)), viewport, Some(rebuilt))

    gate.admit(first, metrics) shouldBe true
    gate.admit(rebuilt, metrics) shouldBe false
    gate.admit(edited, metrics) shouldBe true
    collected.get shouldBe 0
  }

  it should "admit a same-length replacement of the text" in {
    val collected = new AtomicInteger(0)
    val gate      = new AccessibilityPublishGate

    gate.admit(AccessibilitySnapshot.from(stateShowing(counted("hello", collected)), viewport), metrics) shouldBe true
    gate.admit(AccessibilitySnapshot.from(stateShowing(counted("jello", collected)), viewport), metrics) shouldBe true
  }

  "SwingAccessibilityBridge" should "not read the document text until assistive technology asks for it" in {
    val collected = new AtomicInteger(0)
    val canvas    = new JPanel
    val bridge    = new SwingAccessibilityBridge(canvas)

    bridge.publish(AccessibilitySnapshot.from(stateShowing(counted("hello", collected)), viewport), metrics)
    bridge.publish(AccessibilitySnapshot.from(stateShowing(counted("hello!", collected)), viewport), metrics)
    collected.get shouldBe 0

    val document = canvas.getAccessibleContext.getAccessibleChild(0).getAccessibleContext
    document.getAccessibleText.getCharCount shouldBe 6
    document.getAccessibleText.getAtIndex(AccessibleText.CHARACTER, 0) shouldBe "h"
    collected.get should be > 0
  }

  it should "describe the document node with its text only when the description is read" in {
    val collected = new AtomicInteger(0)
    val canvas    = new JPanel
    val bridge    = new SwingAccessibilityBridge(canvas)

    bridge.publish(AccessibilitySnapshot.from(stateShowing(counted("hello", collected)), viewport), metrics)
    collected.get shouldBe 0

    canvas.getAccessibleContext.getAccessibleChild(0).getAccessibleContext.getAccessibleDescription should include(
      "value=hello"
    )
  }

  it should "keep serving the current text once assistive technology has started reading it" in {
    val collected = new AtomicInteger(0)
    val canvas    = new JPanel
    val bridge    = new SwingAccessibilityBridge(canvas)
    bridge.publish(AccessibilitySnapshot.from(stateShowing(counted("hello", collected)), viewport), metrics)
    val proxy = canvas.getAccessibleContext.getAccessibleChild(0)
    proxy.getAccessibleContext.getAccessibleText.getCharCount shouldBe 5

    bridge.publish(AccessibilitySnapshot.from(stateShowing(counted("hello, world", collected)), viewport), metrics)

    canvas.getAccessibleContext.getAccessibleChild(0) should be theSameInstanceAs proxy
    proxy.getAccessibleContext.getAccessibleText.getCharCount shouldBe 12
    proxy.asInstanceOf[JTextComponent].getText shouldBe "hello, world"
  }

  "TuiAccessibilityBridge" should "read the document text once for the title however often the snapshot is published" in {
    val collected = new AtomicInteger(0)
    val written   = ListBuffer.empty[String]
    val bridge    = new TuiAccessibilityBridge(text => written += text)
    val snapshot  = AccessibilitySnapshot.from(stateShowing(counted("hello", collected)), viewport)

    bridge.publish(snapshot)
    bridge.publish(snapshot)

    written.mkString should include("document Untitled document: hello")
    collected.get shouldBe 1
  }
