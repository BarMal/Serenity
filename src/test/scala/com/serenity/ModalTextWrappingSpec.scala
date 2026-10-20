package com.serenity

import java.awt.Font

import com.serenity.config.{AppConfig, LayoutPin, UiLayout}
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.accessibility.{AccessibilityRole, AccessibilitySnapshot}
import com.serenity.ui.layout.*
import com.serenity.ui.renderer.{MeasuredTextAdvances, RendererEntryPoints, ShapedTextCache}
import com.serenity.ui.widget.{Loadable, TextField}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ModalTextWrappingSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val pins = List(LayoutPin("editor.line_numbers", "true", "false"))

  private val overridePrompt = ConfirmPrompt.layoutOverrideAtStartup(UiLayout.Minimal, pins)

  private val longLine =
    "Using the layout removes these settings from your config file, so they follow the layout from then on."

  private def confirm(message: String*): Modal =
    Modal.Confirm(
      ConfirmPrompt.of(
        "Heads up",
        message.toList,
        List(ConfirmChoice("Yes", ConfirmAction.Dismiss), ConfirmChoice("No", ConfirmAction.Dismiss)),
        blocking = true
      )
    )

  private def framed(modal: Modal, width: Int, measure: TextMeasure = TextMeasure.Cells) =
    val frame = LayoutRect(0, 0, width, ModalSurfaceComposition.frameHeight(modal, 1, width, measure = measure))
    frame -> ModalSurfaceComposition.forModal(modal, frame, 1, measure = measure).getOrElse(fail("no composition"))

  private def textRows(plan: ResolvedSurfaceComposition): List[SurfacePaintBox] =
    plan.paintBoxes.filter(box => box.kind == SurfacePaintKind.Text && box.actionId.isEmpty)

  private def words(texts: Iterable[String]): List[String] = texts.flatMap(_.split(' ')).filter(_.nonEmpty).toList

  private def cellsOf(text: String): Int = text.codePoints().map(CharWidth.of(_)).sum()

  "A confirm dialog" should "wrap a long message within the content width, every word in order" in {
    val (frame, plan) = framed(confirm(longLine), 40)

    val rows = textRows(plan).flatMap(_.text)
    rows.foreach(row => cellsOf(row) should be <= 38)
    words(rows) shouldBe words(List("Heads up", longLine))
    rows.size should be > 2
    plan.bounds.width shouldBe 38.0
    frame.width shouldBe 40
  }

  it should "end its text rows above the first choice" in {
    val (_, plan) = framed(confirm(longLine, longLine), 40)

    val textBottom  = textRows(plan).map(_.rect.bottom).max
    val firstChoice = plan.hitRegions.map(_.rect.y).min
    textBottom should be <= firstChoice
    plan.hitRegions.map(_.semanticLabel) shouldBe List("Yes", "No")
    plan.hitRegions.map(_.rect.height).distinct shouldBe List(1.0)
  }

  it should "grow its frame as the width shrinks, and fit every row it grew for" in {
    val modal   = confirm(longLine)
    val heights = List(72, 50, 30, 20).map(ModalSurfaceComposition.frameHeight(modal, 1, _))

    heights.zip(heights.tail).foreach((wider, narrower) => narrower should be >= wider)
    heights.last should be > heights.head
    List(72, 50, 30, 20).foreach { width =>
      val (frame, plan) = framed(modal, width)
      val painted       = words(textRows(plan).flatMap(_.text))
      painted shouldBe words(List("Heads up", longLine))
      plan.paintBoxes.foreach(box => box.rect.bottom should be <= (frame.height - 1).toDouble)
    }
  }

  it should "break a word longer than the width" in {
    val (_, plan) = framed(confirm("x" * 100), 32)

    val rows = textRows(plan).flatMap(_.text).filter(_.startsWith("x"))
    rows.size should be > 3
    rows.foreach(row => row.length should be <= 30)
    rows.mkString shouldBe "x" * 100
  }

  it should "measure wide characters in cells" in {
    val (_, plan) = framed(confirm("日" * 40), 22)

    val rows = textRows(plan).flatMap(_.text).filter(_.startsWith("日"))
    rows.foreach(row => cellsOf(row) should be <= 20)
    rows.mkString shouldBe "日" * 40
    rows.size should be >= 4
  }

  it should "keep a sentence-long layout override line intact on the screen" in {
    val state  = modalState(Modal.Confirm(overridePrompt))
    val layout = com.serenity.ui.layout.LayoutEngine.calculateLayout(state, ViewportSize(100, 30))
    val dialog = state.runtime.modalStack.head
    val rect   = LayoutEngine.calculateModalRect(dialog, state, layout)
    val screen = com.serenity.MockRenderSurface(100, 30)

    RendererEntryPoints.render(
      state,
      cursorVisible = true,
      screen,
      ViewportSize(100, 30),
      com.serenity.state.manager.RenderCaches.create()
    )

    val inside = (rect.y + 1 until rect.bottom - 1).map { y =>
      (rect.x + 1 until rect.right - 1).map(screen.getChar(_, y)).mkString.trim
    }.toList
    inside.mkString(" ").replaceAll(" +", " ") should include(longLine)
    inside.foreach(row => row.length should be <= rect.width - 2)
    inside.exists(_.contains("Use Minimal for all")) shouldBe true
    rect.height should be > ModalSurfaceComposition.frameHeight(Modal.Confirm(overridePrompt), 1, 1000)
  }

  "A replace workflow" should "wrap a long status message in the frame it is sized for" in {
    val status = "The replacement would change 12 lines in 3 files and cannot be undone once the files are saved"
    val modal  = Modal.ReplaceWorkflow(ReplaceWorkflowState(statusMessage = Some(status)))

    val (frame, plan) = framed(modal, 40)

    words(textRows(plan).flatMap(_.text)) shouldBe words(List(status))
    frame.height should be > ModalSurfaceComposition.frameHeight(modal, 1, 200)
    plan.paintBoxes.foreach(box => box.rect.bottom should be <= (frame.height - 1).toDouble)
  }

  it should "paint the wrapped status inside the modal" in {
    val status = "The replacement would change 12 lines in 3 files and cannot be undone once the files are saved"
    val state  = modalState(Modal.ReplaceWorkflow(ReplaceWorkflowState(statusMessage = Some(status))), width = 60)
    val layout = LayoutEngine.calculateLayout(state, ViewportSize(60, 30))
    val rect   = LayoutEngine.calculateModalRect(state.runtime.modalStack.head, state, layout)
    val screen = MockRenderSurface(60, 30)

    RendererEntryPoints.render(
      state,
      cursorVisible = true,
      screen,
      ViewportSize(60, 30),
      com.serenity.state.manager.RenderCaches.create()
    )

    val inside = (rect.y + 1 until rect.bottom - 1).map { y =>
      (rect.x + 1 until rect.right - 1).map(screen.getChar(_, y)).mkString.trim
    }.toList
    inside.mkString(" ").replaceAll(" +", " ") should include(status)
  }

  "A file workflow" should "wrap its status and key hints, and size its frame for the rows they wrap to" in {
    val status = "Create directories: " + List("alpha", "beta", "gamma", "delta", "epsilon").mkString(" / ") +
      " before the file can be written"
    val workflow = FileWorkflowState(
      mode = FileWorkflowMode.SaveAs,
      path = "/tmp/alpha/beta",
      statusMessage = Some(status)
    )
    val modal = Modal.FileWorkflow(workflow)

    val (frame, plan) = framed(modal, 32)

    val rows = textRows(plan).flatMap(_.text)
    rows.foreach(row => cellsOf(row) should be <= 30)
    words(rows) should contain allElementsOf words(List(status))
    frame.height should be > ModalSurfaceComposition.frameHeight(modal, 1, 200)
    plan.paintBoxes.foreach(box => box.rect.bottom should be <= (frame.height - 1).toDouble)
  }

  "A find dialog" should "wrap an invalid regex message above its footer row" in {
    val find = Modal
      .Find(TextField.of("(unclosed group with a long pattern"), Vector.empty, 0)
      .copy(
        options = FindOptions.default.toggled(FindOption.Regex)
      )

    val (frame, plan) = framed(find, 30)

    val errors = textRows(plan).filter(_.tone == OverlayTone.Error).flatMap(_.text)
    errors.size should be > 1
    errors.foreach(row => cellsOf(row) should be <= 28)
    words(errors).take(2) shouldBe List("Invalid", "regex:")
    frame.height should be > ModalSurfaceComposition.frameHeight(find, 1, 200)
    plan.paintBoxes.foreach(box => box.rect.bottom should be <= (frame.height - 1).toDouble)
  }

  "A list picker" should "wrap a long failure message" in {
    val message = "Could not read the session list because the sessions folder is missing or not readable"
    val picker  = ListPicker.of("Open session", Vector.empty, "None").copy(items = Loadable.Failed(message))
    val modal   = Modal.ListPicker(picker)

    val (frame, plan) = framed(modal, 36)

    words(textRows(plan).flatMap(_.text).filter(_ != "Open session")) shouldBe words(List(message))
    frame.height should be > ModalSurfaceComposition.frameHeight(modal, 1, 200)
  }

  "A panel arrangement" should "wrap its key footer" in {
    val modal = Modal.PanelArrangement(PanelArrangement.of(AppState.initial(AppConfig.default)))

    val (frame, plan) = framed(modal, 24)

    val footer = textRows(plan).filter(_.tone == OverlayTone.Muted).flatMap(_.text)
    footer.size should be > 1
    footer.foreach(row => cellsOf(row) should be <= 22)
    frame.height should be > ModalSurfaceComposition.frameHeight(modal, 1, 200)
  }

  "A wrapped confirm dialog" should "keep its choices clickable below the text, where accessibility reports them" in {
    val state   = modalState(Modal.Confirm(overridePrompt), AppConfig.conventional)
    val surface = state.runtime.modalStack.head.id

    val snapshot = AccessibilitySnapshot.from(state, ViewportSize(100, 30))
    val frame    = snapshot.nodes.find(_.id == s"surface:${surface.value}").map(_.bounds).getOrElse(fail("no modal"))
    val plan = ModalSurfaceComposition.forState(Modal.Confirm(overridePrompt), frame, state).getOrElse(fail("no plan"))
    val controls = snapshot.nodes.filter(_.id.startsWith(s"surface:${surface.value}/control:"))

    controls.map(_.role).distinct shouldBe List(AccessibilityRole.Button)
    controls.map(_.name) shouldBe overridePrompt.choices.items.map(_.label).toList
    controls.foreach { node =>
      node.bounds.y should be >= textRows(plan).map(_.rect.bottom).max.toInt
      node.bounds.bottom should be <= frame.bottom - 1
    }
    plan.hitRegions.foreach(hit => plan.hitAt(hit.rect.x + 1, hit.rect.y).map(_.focusId) shouldBe Some(hit.focusId))
  }

  "Pixel wrapping" should "keep every row of a dialog within the content's pixel width" in {
    val font     = Font(Font.SANS_SERIF, Font.PLAIN, 14)
    val advances = MeasuredTextAdvances(MeasuredTextAdvances.DefaultCapacity, ShapedTextCache())
    val measure: TextMeasure.Pixels = TextMeasure.Pixels(
      font,
      TextLayoutSnapshot.defaultFontRenderContext(),
      CellMetrics.fromFont(Font(Font.MONOSPACED, Font.PLAIN, 14)).charWidth,
      0.0,
      advances
    )
    val modal = Modal.Confirm(overridePrompt)

    List(72, 50, 34).foreach { width =>
      val (frame, plan) = framed(modal, width, measure)
      val rows          = textRows(plan).flatMap(_.text)
      rows.foreach { row =>
        advances.advanceOf(row, font, measure.fontRenderContext) should be <=
          ((width - 2) * measure.cellPx).toFloat
      }
      words(rows) shouldBe words(overridePrompt.title :: overridePrompt.message)
      plan.paintBoxes.foreach(box => box.rect.bottom should be <= (frame.height - 1).toDouble)
    }
  }

  private def modalState(modal: Modal, config: AppConfig = AppConfig.default, width: Int = 100): AppState =
    val initial = AppState.initial(config)
    initial.copy(
      persisted = initial.persisted.copy(focus = Focus.Modal),
      runtime = initial.runtime.copy(
        modalStack = List(ModalDialog(SurfaceId("wrapped-modal"), modal, ModalPlacement.Centered)),
        viewportSize = Some(ViewportSize(width, 30))
      )
    )
