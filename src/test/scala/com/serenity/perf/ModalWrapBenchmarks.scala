package com.serenity.perf

import com.serenity.config.{AppConfig, LayoutPin, UiLayout}
import com.serenity.rope.Balance
import com.serenity.state.models.{
  AppState,
  ConfirmAction,
  ConfirmChoice,
  ConfirmPrompt,
  FileWorkflowMode,
  FileWorkflowState,
  FileWorkflowSuggestion,
  Modal
}
import com.serenity.ui.layout.{LayoutRect, ModalSurfaceComposition, TextMeasure}

/** What wrapping a dialog's prose costs a frame, wrapped by cells against by the GUI font's pixels: the frame height a
  * layout pass asks for plus the composition a paint pass builds, each wrapping the same text once. `nowrap` is a frame
  * wide enough that nothing wraps, which is what every frame cost before dialogs wrapped. `cold` makes every call a
  * text the pixel measurements have not seen, as the first frame of a changed prompt is.
  */
private[perf] object ModalWrapBenchmarks:

  given Balance = Balance.default

  private val FrameWidth = 72

  private val pins = List(
    LayoutPin("editor.line_numbers", "true", "false"),
    LayoutPin("ui.status_line", "pinned", "hidden"),
    LayoutPin("editor.minimap", "true", "false")
  )

  private val layoutOverride = Modal.Confirm(ConfirmPrompt.layoutOverrideAtStartup(UiLayout.Minimal, pins))

  private def tagged(text: String, tag: String): String = text.split(' ').map(_ + tag).mkString(" ")

  private def longConfirm(tag: String): Modal.Confirm =
    Modal.Confirm(
      ConfirmPrompt.of(
        tagged("Reload notes.md from disk", tag),
        List(
          tagged(
            "The file changed on disk while you had unsaved edits, so reloading would throw those edits away and " +
              "keeping them would hide what changed, which is why both choices are offered here.",
            tag
          ),
          "",
          tagged(
            "Reload replaces the buffer with what is on disk. Keep mine leaves the buffer as it is, and Compare " +
              "opens the two side by side so each difference can be taken or left one at a time.",
            tag
          )
        ),
        List(
          ConfirmChoice("Reload", ConfirmAction.Dismiss),
          ConfirmChoice("Keep mine", ConfirmAction.Dismiss),
          ConfirmChoice("Compare", ConfirmAction.Dismiss)
        ),
        blocking = true
      )
    )

  private val fileWorkflow = Modal.FileWorkflow(
    FileWorkflowState(
      mode = FileWorkflowMode.SaveAs,
      filename = "notes.md",
      path = "/home/someone/projects/serenity/docs/drafts",
      suggestions = List("one", "two", "three", "four").map(FileWorkflowSuggestion(_, isDirectory = true)),
      missingPathSegments = List("projects", "serenity", "docs", "drafts"),
      confirmCreateDirectories = true
    )
  )

  private def frame(modal: Modal, width: Int, measure: TextMeasure): (LayoutRect, Int) =
    val height = ModalSurfaceComposition.frameHeight(modal, 1, width, measure = measure)
    val rect   = LayoutRect(0, 0, width, height)
    rect -> ModalSurfaceComposition.forModal(modal, rect, 1, measure = measure).fold(0)(_.paintBoxes.size)

  def benchmarks(): List[BenchmarkRunner.Benchmark] =
    val pixels = TextMeasure.pixelsFor(AppState.initial(AppConfig.default))
    val modals = List(
      "layout_override" -> layoutOverride,
      "long_confirm"    -> longConfirm(""),
      "file_workflow"   -> fileWorkflow
    )
    val settled = modals.flatMap { (name, modal) =>
      List(
        ("nowrap", TextMeasure.Cells, 1000),
        ("cells", TextMeasure.Cells, FrameWidth),
        ("pixels", pixels, FrameWidth)
      ).map { (strategy, measure, width) =>
        BenchmarkRunner.Benchmark(
          s"render.modal_wrap.$name.$strategy",
          200,
          400,
          () => assert(frame(modal, width, measure)._2 > 0),
          () => frame(modal, width, measure),
          minBatch = 50,
          settleJit = true
        )
      }
    }
    val counter = java.util.concurrent.atomic.AtomicInteger(0)
    val cold = List(("cells", TextMeasure.Cells), ("pixels", pixels)).map { (strategy, measure) =>
      BenchmarkRunner.Benchmark(
        s"render.modal_wrap.long_confirm.cold_$strategy",
        200,
        400,
        () => assert(frame(longConfirm("0"), FrameWidth, measure)._2 > 0),
        () => frame(longConfirm(counter.incrementAndGet().toString), FrameWidth, measure),
        minBatch = 10,
        settleJit = true
      )
    }
    settled ++ cold

  def main(args: Array[String]): Unit =
    BenchmarkRunner.printResults(BenchmarkRunner.runMatching(args.toList, benchmarks()))
end ModalWrapBenchmarks
