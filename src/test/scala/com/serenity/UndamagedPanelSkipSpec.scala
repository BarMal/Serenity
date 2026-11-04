package com.serenity

import com.serenity.state.manager.DamageProducer
import com.serenity.state.models.*
import com.serenity.ui.layout.LayoutRect
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A frame that damages nothing a floating panel shows leaves the panel's pixels where the persisting image already
  * holds them, and the image still equals one drawn from scratch. The two pooled images alternate, as `SwingWindow`
  * alternates them, so a panel repainted on one image is stale on the other until that is repainted too.
  */
class UndamagedPanelSkipSpec extends AnyFlatSpec with Matchers with PersistedFrameFixture:

  private val scale  = 2.0
  private val second = BufferId(2)
  private val lines  = codeLines

  private def withTabs(state: AppState): AppState =
    state.copy(persisted =
      state.persisted.copy(
        buffers = state.persisted.buffers.updated(second, Buffer.fromString(second, "second file")),
        bufferOrder = state.persisted.bufferOrder :+ second
      )
    )

  private val start: AppState = withTabs(stateOf(lines.mkString("\n"), CursorPosition(10, 0)))

  private def devicePixels(cells: LayoutRect): (Int, Int, Int, Int) =
    (
      math.round(cellMetrics.toPixelX(cells.x) * scale).toInt,
      math.round(cellMetrics.toPixelY(cells.y) * scale).toInt,
      math.round(cellMetrics.toPixelX(cells.width) * scale).toInt,
      math.round(cellMetrics.toPixelY(cells.height) * scale).toInt
    )

  private val Sentinel = 0x123456

  private def stamped(image: FramePixels, rect: (Int, Int, Int, Int)): Seq[(Int, Int)] =
    val (left, top, width, height) = rect
    val points = for dx <- List(width / 4, width / 2, 3 * width / 4); dy <- List(height / 2) yield (left + dx, top + dy)
    points.foreach((x, y) => image.stamp(x, y, Sentinel))
    points

  private val typed: List[AppState] =
    List.iterate(start, 5)(state => typedAt(state, lines, 12, "x")).drop(1)

  "A frame after a keystroke" should "leave an undamaged tab bar where the persisting image holds it, on both images" in {
    val session = new Session(scale)
    val plan    = session.settle(start)
    val tabBar  = plan.layoutContract.tabBarRect.getOrElse(fail("expected a tab bar"))
    val rect    = devicePixels(tabBar)
    val _ = typed.foldLeft(start) { (before, after) =>
      val image      = session.pendingImage
      val points     = stamped(image, rect)
      val (drawn, _) = session.draw(after, DamageProducer.forTransition(before, after))
      points.foreach((x, y) => withClue(s"pixel $x,$y: ")((drawn.rgbAt(x, y) & 0xffffff) shouldBe Sentinel))
      after
    }
  }

  it should "show the same pixels as a frame drawn from scratch, on both images" in {
    val session = new Session(scale)
    val _       = session.settle(start)
    val _ = typed.foldLeft(start) { (before, after) =>
      val (drawn, _) = session.draw(after, DamageProducer.forTransition(before, after))
      withClue("after typing: ")(differingPixels(drawn, cold(after, scale)) shouldBe empty)
      after
    }
  }

  private def withSecondDirty(state: AppState, dirty: Boolean): AppState =
    val buffer = state.persisted.buffers(second)
    state.copy(persisted =
      state.persisted.copy(buffers =
        state.persisted.buffers.updated(second, buffer.copy(document = buffer.document.copy(isDirty = dirty)))
      )
    )

  private def withThirdTab(state: AppState): AppState =
    val third = BufferId(3)
    state.copy(persisted =
      state.persisted.copy(
        buffers = state.persisted.buffers.updated(third, Buffer.fromString(third, "third file")),
        bufferOrder = state.persisted.bufferOrder :+ third
      )
    )

  private def withDarkTheme(state: AppState): AppState =
    state.copy(persisted = state.persisted.copy(theme = com.serenity.ui.theme.Theme.dark))

  private val changes: List[(String, AppState => AppState)] = List(
    "a tab marked modified" -> (withSecondDirty(_, dirty = true)),
    "a tab opened"          -> withThirdTab,
    "a different theme"     -> withDarkTheme
  )

  for (label, change) <- changes do
    s"A tab bar the frame damages ($label)" should "be repainted on each image, then kept again, and match a fresh frame" in {
      val session = new Session(scale)
      val _       = session.settle(start)
      val changed = change(start)
      val steps   = List.iterate(changed, 5)(state => typedAt(state, lines, 12, "x"))
      val _ = steps.foldLeft(start) { (before, after) =>
        val damage =
          if label == "a different theme" && (after eq changed) then Damage.Everything
          else DamageProducer.forTransition(before, after)
        val (drawn, _) = session.draw(after, damage)
        withClue(s"$label, frame ${steps.indexOf(after)}: ")(differingPixels(drawn, cold(after, scale)) shouldBe empty)
        after
      }
    }

  "The first frames on a fresh window" should "match a frame drawn from scratch, whichever image they land on" in {
    val session = new Session(scale)
    val _ = typed.foldLeft((start, true)) {
      case ((before, first), after) =>
        val damage     = if first then Damage.Everything else DamageProducer.forTransition(before, after)
        val (drawn, _) = session.draw(after, damage)
        withClue("first frames: ")(differingPixels(drawn, cold(after, scale)) shouldBe empty)
        (after, false)
    }
  }
