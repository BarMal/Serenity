package com.serenity

import com.serenity.diagnostics.FrameCounter
import com.serenity.state.manager.DamageProducer
import com.serenity.state.models.*
import com.serenity.ui.color.RenderColor
import com.serenity.ui.layout.{PixelRect, ScrollThumb, ScrollbarGeometry, ScrollbarLayout}
import com.serenity.ui.renderer.EditorPaneRenderPlan
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The scrollbar at the laptop's 2x scale: a thin strip drawn over the right edge of the text, repainted without the
  * rows being repainted for its sake, and equal to a frame drawn from scratch to the last device pixel -- the text
  * beneath it included.
  */
class ScrollbarRenderSpec extends AnyFlatSpec with Matchers with PersistedFrameFixture:

  private val scale  = 2.0
  private val colors = Theme.light.scrollbar

  private def rgb(color: RenderColor): Int = (color.red << 16) | (color.green << 8) | color.blue

  private def track(plan: EditorPaneRenderPlan): PixelRect =
    ScrollbarLayout.strip(
      plan.paneLayouts.getOrElse(paneId, fail("expected the pane's layout")).contentRect,
      cellMetrics
    )

  private def thumb(plan: EditorPaneRenderPlan): Option[ScrollThumb] =
    plan.scrolls.get(paneId).flatMap(ScrollbarGeometry.thumb(_, track(plan).heightPx))

  /** The device pixel at the middle of the thumb, `below` logical pixels below its start (or above, negative). */
  private def thumbPixel(plan: EditorPaneRenderPlan, below: Int): (Int, Int) =
    val bar = track(plan)
    val at  = thumb(plan).getOrElse(fail("expected a thumb"))
    (
      ((bar.xPx + bar.widthPx / 2) * scale).toInt,
      ((bar.yPx + at.offsetPx + below) * scale).toInt
    )

  private def colorAt(image: FramePixels, at: (Int, Int)): Int = image.rgbAt(at._1, at._2) & 0xffffff

  private val longDocument = codeLines.mkString("\n")

  /** Rows that run the full width of the text, so the glyphs reach the strip the bar is drawn over. */
  private val fullWidthDocument = Vector.tabulate(60)(line => s"$line " + "abcdefghij" * 40).mkString("\n")

  private def viewingMiddle: AppState = stateOf(longDocument, CursorPosition(30, 0), topLine = 10)

  private def viewingFullWidthRows: AppState = stateOf(fullWidthDocument, CursorPosition(0, 0), topLine = 2)

  private def drawnFrom(before: AppState, after: AppState): FramePixels =
    val session = Session()
    val _       = session.settle(before)
    session.draw(after, DamageProducer.forTransition(before, after))._1

  private def held(state: AppState): AppState =
    state.copy(runtime =
      state.runtime.copy(pointerGesture =
        state.runtime.pointerGesture.copy(scrollbarGrab = Some(ScrollbarGrab(paneId, grabOffsetPx = 6)))
      )
    )

  private def withoutScrollbar(state: AppState): AppState =
    val config = state.persisted.config
    state.copy(persisted =
      state.persisted.copy(config = config.copy(surfaceConfig = config.surfaceConfig.copy(scrollbarEnabled = false)))
    )

  "A frame with the scrollbar turned off" should "paint no thumb and plan no scroll for the pane" in {
    val off           = withoutScrollbar(viewingMiddle)
    val (image, plan) = Session().draw(off, Damage.Everything)
    val bar           = track(plan)
    val middle        = (((bar.xPx + bar.widthPx / 2) * scale).toInt, ((bar.yPx + bar.heightPx / 4) * scale).toInt)

    plan.scrolls shouldBe empty
    withClue("where the thumb would be: ")(colorAt(image, middle) shouldBe rgb(Theme.light.background))
    differingPixels(image, cold(off)) shouldBe empty
  }

  "A first frame" should "paint the thumb in the theme's colour and leave the rest of the strip to the text" in {
    val (image, plan) = Session().draw(viewingMiddle, Damage.Everything)

    withClue("thumb: ")(colorAt(image, thumbPixel(plan, below = 10)) shouldBe rgb(colors.thumb))
    withClue("strip above the thumb: ")(
      colorAt(image, thumbPixel(plan, below = -6)) shouldBe rgb(Theme.light.background)
    )
  }

  it should "paint no thumb when the whole document is in view" in {
    val short         = stateOf("one\ntwo\nthree", CursorPosition(0, 0))
    val (image, plan) = Session().draw(short, Damage.Everything)
    val bar           = track(plan)

    thumb(plan) shouldBe None
    withClue("middle of the strip: ")(
      colorAt(
        image,
        (((bar.xPx + bar.widthPx / 2) * scale).toInt, ((bar.yPx + bar.heightPx / 2) * scale).toInt)
      ) shouldBe rgb(Theme.light.background)
    )
  }

  it should "equal a frame drawn from scratch" in {
    val (image, _) = Session().draw(viewingMiddle, Damage.Everything)

    differingPixels(image, cold(viewingMiddle)) shouldBe empty
  }

  it should "wrap the text where it did without a bar, leaving the strip over the last column" in {
    val (_, plan) = Session().draw(viewingMiddle, Damage.Everything)
    val content   = plan.paneLayouts.getOrElse(paneId, fail("expected the pane's layout")).contentRect

    track(plan).rightPx shouldBe cellMetrics.toPixelX(content.right)
    track(plan).xPx should be >= cellMetrics.toPixelX(content.right - 1)
  }

  "A scroll" should "move the thumb with the view and equal a frame drawn from scratch" in {
    val before  = viewingMiddle
    val after   = scrolledTo(before, 14)
    val session = Session()
    val first   = session.settle(before)

    val (image, plan) = session.draw(after, DamageProducer.forTransition(before, after))

    val (oldAt, newAt) = (thumb(first).getOrElse(fail("expected a thumb")), thumb(plan).getOrElse(fail("expected one")))
    newAt.offsetPx should be > oldAt.offsetPx
    withClue("thumb at its new place: ")(colorAt(image, thumbPixel(plan, below = 1)) shouldBe rgb(colors.thumb))
    withClue("vacated strip: ")(colorAt(image, thumbPixel(first, below = 1)) shouldBe rgb(Theme.light.background))
    differingPixels(image, cold(after)) shouldBe empty
  }

  it should "present the bar's strip along with the rows" in {
    val before  = viewingMiddle
    val after   = scrolledTo(before, 14)
    val session = Session()
    val first   = session.settle(before)

    val _ = session.draw(after, DamageProducer.forTransition(before, after))

    val bar = track(first)
    session.repaintRegion.getOrElse(fail("expected a bounded repaint")).exists(_.intersects(bar)) shouldBe true
  }

  "A scroll over rows of text that reach the strip" should "put the text back where the thumb was, as a frame from scratch has it" in {
    val before  = viewingFullWidthRows
    val after   = scrolledTo(before, 20)
    val session = Session()
    val first   = session.settle(before)

    val (image, plan) = session.draw(after, DamageProducer.forTransition(before, after))

    thumb(plan).map(_.offsetPx) should not be thumb(first).map(_.offsetPx)
    differingPixels(image, cold(after)) shouldBe empty
  }

  it should "equal a frame from scratch after several scrolls in a row" in {
    val states  = Seq(2, 9, 17, 4, 30).map(scrolledTo(viewingFullWidthRows, _))
    val session = Session()
    val _       = session.settle(viewingFullWidthRows)

    val last = states.foldLeft((viewingFullWidthRows, Option.empty[FramePixels])) {
      case ((before, _), after) =>
        (after, Some(session.draw(after, DamageProducer.forTransition(before, after))._1))
    }

    differingPixels(last._2.getOrElse(fail("expected a frame")), cold(last._1)) shouldBe empty
  }

  "An edit that leaves the thumb where it was" should "present less than the bar's whole strip, and equal a frame from scratch" in {
    val before  = viewingMiddle
    val after   = typedAt(before, codeLines, line = 30, text = "x")
    val session = Session()
    val plan    = session.settle(before)

    val (image, _) = session.draw(after, DamageProducer.forTransition(before, after))

    session.repaintRegion
      .getOrElse(fail("expected a bounded repaint"))
      .forall(_.heightPx < track(plan).heightPx) shouldBe
      true
    differingPixels(image, cold(after)) shouldBe empty
  }

  "An edit whose rows miss the thumb" should "leave the thumb's pixels as they were, lifting and drawing nothing" in {
    val before  = viewingMiddle
    val after   = typedAt(before, codeLines, line = 30, text = "x")
    val session = Session()
    val plan    = session.settle(before)
    val at      = thumbPixel(plan, below = 10)
    session.pendingImage.stamp(at._1, at._2, 0xff00ff)

    val (image, _) = session.draw(after, DamageProducer.forTransition(before, after))

    withClue("the stamp in the thumb: ")(colorAt(image, at) shouldBe 0xff00ff)
  }

  "An edit whose rows cross the thumb" should "draw the thumb again over the repainted text, as a frame from scratch has it" in {
    val before  = viewingMiddle
    val after   = typedAt(before, codeLines, line = 12, text = "x")
    val session = Session()
    val plan    = session.settle(before)
    val at      = thumbPixel(plan, below = 10)
    session.pendingImage.stamp(at._1, at._2, 0xff00ff)

    val (image, _) = session.draw(after, DamageProducer.forTransition(before, after))

    withClue("the stamp in the thumb: ")(colorAt(image, at) shouldBe rgb(colors.thumb))
    differingPixels(image, cold(after)) shouldBe empty
  }

  it should "leave the part of a tall thumb the edit does not reach as it was" in {
    val before  = viewingMiddle
    val after   = typedAt(before, codeLines, line = 11, text = "x")
    val session = Session()
    val plan    = session.settle(before)
    val far     = thumbPixel(plan, below = thumb(plan).getOrElse(fail("expected a thumb")).lengthPx - 6)
    session.pendingImage.stamp(far._1, far._2, 0xff00ff)

    val (image, _) = session.draw(after, DamageProducer.forTransition(before, after))

    withClue("the stamp at the far end of the thumb: ")(colorAt(image, far) shouldBe 0xff00ff)
  }

  "An edit below the view that moves the thumb" should "repaint no row for the bar's sake, and equal a frame from scratch" in {
    val before  = viewingMiddle
    val moves   = typedAt(before, codeLines, line = 100, text = "\n" * 20)
    val stays   = typedAt(before, codeLines, line = 100, text = "x")
    val session = Session()
    val _       = session.settle(before)

    val (image, _)    = session.draw(moves, DamageProducer.forTransition(before, moves))
    val movedCounts   = session.lastFrame
    val steadySession = Session()
    val _             = steadySession.settle(before)
    val _             = steadySession.draw(stays, DamageProducer.forTransition(before, stays))
    val steadyCounts  = steadySession.lastFrame

    movedCounts(FrameCounter.RowsDrawn) shouldBe steadyCounts(FrameCounter.RowsDrawn)
    differingPixels(image, cold(moves)) shouldBe empty
  }

  "Taking hold of the thumb" should "repaint it in the held colour and present only the bar's strip" in {
    val before  = viewingMiddle
    val after   = held(before)
    val session = Session()
    val plan    = session.settle(before)

    val (image, _) = session.draw(after, DamageProducer.forTransition(before, after))

    withClue("thumb: ")(colorAt(image, thumbPixel(plan, below = 10)) shouldBe rgb(colors.thumbHeld))
    session.repaintRegion shouldBe Some(List(track(plan)))
    session.lastFrame(FrameCounter.RowsDrawn) shouldBe 0L
    differingPixels(image, cold(after)) shouldBe empty
  }

  "Letting go of the thumb" should "paint it in the resting colour again" in {
    val before  = held(viewingMiddle)
    val after   = viewingMiddle
    val session = Session()
    val plan    = session.settle(before)

    val (image, _) = session.draw(after, DamageProducer.forTransition(before, after))

    withClue("thumb: ")(colorAt(image, thumbPixel(plan, below = 10)) shouldBe rgb(colors.thumb))
    differingPixels(image, cold(after)) shouldBe empty
  }

  "A frame nothing damaged" should "draw no row, present nothing and equal a frame from scratch" in {
    val state   = viewingMiddle
    val session = Session()
    val _       = session.settle(state)

    val (image, _) = session.draw(state, Damage.Nothing)

    session.lastFrame(FrameCounter.RowsDrawn) shouldBe 0L
    session.repaintRegion.forall(_.isEmpty) shouldBe true
    differingPixels(image, cold(state)) shouldBe empty
  }
