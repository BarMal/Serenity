package com.serenity.ui.widget

import java.lang.management.ManagementFactory

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SelectableListSpec extends AnyFlatSpec with Matchers:

  private val letters = SelectableList.of(('a' to 'j').map(_.toString))
  private val rows    = 4

  private def after(list: SelectableList[String], inputs: WidgetInput*): SelectableList[String] =
    inputs.foldLeft(list)((current, input) => current.update(input, rows)._1)

  private val allocationBean = ManagementFactory.getThreadMXBean match
    case bean: com.sun.management.ThreadMXBean if bean.isThreadAllocatedMemorySupported =>
      if !bean.isThreadAllocatedMemoryEnabled then bean.setThreadAllocatedMemoryEnabled(true)
      Some(bean)
    case _ => None

  /** Warms up first so the sample measures JIT-compiled code, as `UndoStatePerformanceSpec` does. */
  private def allocatedBytesForVisible(list: SelectableList[String]): Option[Long] =
    allocationBean.map { bean =>
      val threadId = Thread.currentThread().threadId()
      (1 to 200).foreach(_ => assert(list.visible(rows).nonEmpty))
      val before  = bean.getThreadAllocatedBytes(threadId)
      val visible = list.visible(rows)
      val after   = bean.getThreadAllocatedBytes(threadId)
      assert(visible.size == rows)
      after - before
    }

  "A selectable list" should "start on its first item and move with the arrows" in {
    after(letters, WidgetInput.Down, WidgetInput.Down).selectedItem shouldBe Some("c")
    after(letters, WidgetInput.Down, WidgetInput.Up).selectedItem shouldBe Some("a")
  }

  it should "wrap past either end, or stop there when told to" in {
    after(letters, WidgetInput.Up).selectedItem shouldBe Some("j")
    after(letters.copy(endBehaviour = EndBehaviour.Stop), WidgetInput.Up).selectedItem shouldBe Some("a")
    after(letters, WidgetInput.Last, WidgetInput.Down).selectedItem shouldBe Some("a")
  }

  it should "page by a viewport less one row, without wrapping" in {
    after(letters, WidgetInput.PageDown).selectedItem shouldBe Some("d")
    after(
      letters,
      WidgetInput.PageDown,
      WidgetInput.PageDown,
      WidgetInput.PageDown,
      WidgetInput.PageDown
    ).selectedItem shouldBe Some("j")
    after(letters, WidgetInput.PageUp).selectedItem shouldBe Some("a")
  }

  it should "jump to the first and last items" in {
    after(letters, WidgetInput.Last).selectedItem shouldBe Some("j")
    after(letters, WidgetInput.Last, WidgetInput.First).selectedItem shouldBe Some("a")
  }

  it should "scroll the selection into view, and no further" in {
    val atSixth =
      after(letters, WidgetInput.Down, WidgetInput.Down, WidgetInput.Down, WidgetInput.Down, WidgetInput.Down)
    atSixth.offset shouldBe 2
    atSixth.visible(rows).map(_._1) shouldBe Vector("c", "d", "e", "f")
    after(atSixth, WidgetInput.Up, WidgetInput.Up, WidgetInput.Up).offset shouldBe 2
    after(atSixth, WidgetInput.First).offset shouldBe 0
    after(letters, WidgetInput.Last).offset shouldBe 6
  }

  it should "scroll with the wheel without moving the selection, clamped to the content" in {
    val scrolled = after(letters, WidgetInput.Scroll(3))
    scrolled.offset shouldBe 3
    scrolled.selectedItem shouldBe Some("a")
    after(letters, WidgetInput.Scroll(50)).offset shouldBe 6
    after(letters, WidgetInput.Scroll(-5)).offset shouldBe 0
  }

  it should "bring a scrolled-away selection back into view when it moves" in {
    after(letters, WidgetInput.Scroll(5), WidgetInput.Down).offset shouldBe 1
  }

  it should "activate the selected item, and report a dismissal" in {
    letters.update(WidgetInput.Activate, rows)._2 shouldBe Some(ListOutcome.Activated(0, "a"))
    letters.update(WidgetInput.Dismiss, rows)._2 shouldBe Some(ListOutcome.Dismissed)
    SelectableList.of(Seq.empty[String]).update(WidgetInput.Activate, rows)._2 shouldBe None
  }

  it should "select on a click and activate on a double-click" in {
    val (clicked, none) = letters.update(WidgetInput.Click(2, clicks = 1), rows)
    clicked.selectedItem shouldBe Some("c")
    none shouldBe None
    letters.update(WidgetInput.Click(2, clicks = 2), rows)._2 shouldBe Some(ListOutcome.Activated(2, "c"))
  }

  it should "track hover apart from selection" in {
    val hovered = after(letters, WidgetInput.Hover(Some(3)))
    hovered.hovered shouldBe Some(3)
    hovered.selectedItem shouldBe Some("a")
    after(letters, WidgetInput.Hover(Some(99))).hovered shouldBe None
  }

  it should "keep its selection on the same item when its items change" in {
    val onD      = after(letters, WidgetInput.Down, WidgetInput.Down, WidgetInput.Down)
    val reloaded = onD.withItems(Vector("z", "d", "e"), rows)(_ == _)
    reloaded.selectedItem shouldBe Some("d")
    onD.withItems(Vector("x", "y"), rows)(_ == _).selectedItem shouldBe Some("y")
    onD.withItems(Vector.empty[String], rows)(_ == _).selected shouldBe None
  }

  it should "show the viewport's items with their indices, clipped at either end" in {
    letters.copy(offset = 8).visible(rows) shouldBe Vector(("i", 8), ("j", 9))
    letters.copy(offset = -2).visible(rows) shouldBe Vector(("a", 0), ("b", 1))
    letters.copy(offset = 3).visible(0) shouldBe Vector(("d", 3))
    SelectableList.of(Seq.empty[String]).visible(rows) shouldBe Vector.empty
  }

  it should "touch only the visible range when listing what the viewport shows" in {
    val small = SelectableList.of((0 until 10).map(_.toString)).copy(offset = 5)
    val large = SelectableList.of((0 until 200_000).map(_.toString)).copy(offset = 100_000)
    (allocatedBytesForVisible(small), allocatedBytesForVisible(large)) match
      case (Some(smallBytes), Some(largeBytes)) =>
        // Zipping every item before slicing allocates a tuple and a boxed index per item, megabytes here.
        withClue(s"10 items allocated ${smallBytes}B, 200,000 items allocated ${largeBytes}B: ") {
          largeBytes should be < 64_000L
        }
      case _ =>
        info("JVM per-thread allocation counter unsupported on this runtime -- skipping the allocation assertion")
  }

  it should "ignore input it has no meaning for" in {
    letters.update(WidgetInput.Insert('q'), rows) shouldBe (letters, None)
  }
end SelectableListSpec
