package com.serenity

import java.awt.Font

import com.serenity.ui.layout.{
  LogicalPixelRect,
  ResolvedSurfaceComposition,
  SurfaceActionId,
  SurfaceActionItem,
  SurfaceComposition,
  SurfaceCompositionError,
  SurfaceCompositionMetrics,
  SurfaceFocusId,
  SurfacePrimitive,
  TextLayoutSnapshot
}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SurfaceCompositionSpec extends AnyFlatSpec with Matchers:

  private val font = Font(Font.SANS_SERIF, Font.PLAIN, 16)

  private val metrics = SurfaceCompositionMetrics.fromFont(
    font,
    TextLayoutSnapshot.defaultFontRenderContext()
  )

  private def resolved(
    content: SurfacePrimitive,
    bounds: LogicalPixelRect = LogicalPixelRect(10.25, 20.5, 320.0, 180.0)
  ): ResolvedSurfaceComposition =
    SurfaceComposition
      .layout(content, bounds, metrics)
      .fold(errors => fail(errors.mkString(", ")), identity)

  "SurfaceComposition" should "share exact paint and hit rectangles for every interactive child" in {
    val inputFocus = SurfaceFocusId("search")
    val openFocus  = SurfaceFocusId("open")
    val closeFocus = SurfaceFocusId("close")
    val content = SurfacePrimitive.Column(
      List(
        SurfacePrimitive.TextInput(
          value = "needle",
          focusId = inputFocus,
          semanticLabel = "Search query",
          minimumWidthPx = 120.0,
          cursorOffset = Some(3)
        ),
        SurfacePrimitive.ActionList(
          List(
            SurfaceActionItem("Open", SurfaceActionId("open"), openFocus, "Open document"),
            SurfaceActionItem("Close", SurfaceActionId("close"), closeFocus, "Close document", selected = true)
          )
        )
      )
    )

    val layout = resolved(content)

    layout.focusOrder shouldBe List(inputFocus, openFocus, closeFocus)
    layout.hitRegions.map(_.semanticLabel) shouldBe List("Search query", "Open document", "Close document")
    layout.paintBoxes.find(_.focusId.contains(inputFocus)).flatMap(_.cursorOffset) shouldBe Some(3)
    layout.paintBoxes.find(_.focusId.contains(closeFocus)).exists(_.selected) shouldBe true
    layout.hitRegions.foreach { hit =>
      val paint = layout.paintBoxes.find(_.focusId.contains(hit.focusId)).getOrElse(fail(s"missing paint for $hit"))
      paint.rect shouldBe hit.rect
      layout.hitAt(hit.rect.x + 0.25, hit.rect.y + 0.25) shouldBe Some(hit)
    }
  }

  it should "clip nested rows and columns to their parent content rectangle" in {
    val bounds = LogicalPixelRect(4.5, 7.25, 90.0, metrics.lineHeightPx * 2.0)
    val content = SurfacePrimitive.Column(
      List(
        SurfacePrimitive.Row(
          List(
            SurfacePrimitive.Text("A very long proportional label"),
            SurfacePrimitive.Spacer(widthPx = 12.0, heightPx = metrics.lineHeightPx),
            SurfacePrimitive.Text("trailing")
          )
        ),
        SurfacePrimitive.Row(
          List(
            SurfacePrimitive.Spacer(widthPx = 10.0, heightPx = metrics.lineHeightPx),
            SurfacePrimitive.TextInput(
              value = "value",
              focusId = SurfaceFocusId("value"),
              semanticLabel = "Value",
              minimumWidthPx = 120.0
            )
          )
        ),
        SurfacePrimitive.Text("clipped third row")
      )
    )

    val layout = resolved(content, bounds)

    layout.paintBoxes should not be empty
    layout.paintBoxes.foreach(box => bounds.containsRect(box.rect) shouldBe true)
    layout.hitRegions.foreach(hit => bounds.containsRect(hit.rect) shouldBe true)
    layout.paintBoxes.exists(_.text.contains("clipped third row")) shouldBe false
  }

  it should "measure intrinsic text widths with the proportional UI font" in {
    val narrow = resolved(SurfacePrimitive.Text("iiii")).intrinsicSize.width
    val wide   = resolved(SurfacePrimitive.Text("WWWW")).intrinsicSize.width

    wide should be > narrow
  }

  it should "retain fractional Hi-DPI coordinates for pixel hit testing" in {
    val focus  = SurfaceFocusId("query")
    val bounds = LogicalPixelRect(10.25, 20.5, 160.75, metrics.lineHeightPx + 0.5)
    val layout = resolved(
      SurfacePrimitive.TextInput("query", focus, "Query", minimumWidthPx = 100.5),
      bounds
    )
    val hit = layout.hitRegions.headOption.getOrElse(fail("expected input hit region"))

    hit.rect.x shouldBe 10.25
    hit.rect.y shouldBe 20.5
    hit.rect.width shouldBe 100.5
    layout.hitAt(hit.rect.x + 0.01, hit.rect.y + 0.01).map(_.focusId) shouldBe Some(focus)
    layout.hitAt(hit.rect.right, hit.rect.y + 0.01) shouldBe None
  }

  it should "reject interactive primitives without semantic labels or stable IDs" in {
    val invalid = SurfacePrimitive.Column(
      List(
        SurfacePrimitive.TextInput("query", SurfaceFocusId(""), "", minimumWidthPx = 40.0),
        SurfacePrimitive.ActionList(
          List(SurfaceActionItem("Run", SurfaceActionId(""), SurfaceFocusId("run"), ""))
        )
      )
    )

    SurfaceComposition.layout(invalid, LogicalPixelRect(0, 0, 100, 100), metrics) shouldBe Left(
      List(
        SurfaceCompositionError.EmptyFocusId,
        SurfaceCompositionError.EmptySemanticLabel,
        SurfaceCompositionError.EmptyActionId,
        SurfaceCompositionError.EmptySemanticLabel
      )
    )
  }
