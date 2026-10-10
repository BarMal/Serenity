package com.serenity.text

import java.nio.file.Path

import com.serenity.io.ProjectFileListing
import com.serenity.keystroke.events.ModalInsertChar
import com.serenity.rope.{Balance, Rope, RopeCharacterSource, nextGraphemeBoundary, previousGraphemeBoundary}
import com.serenity.state.models.*
import com.serenity.state.reducers.{ModalEventReducer, ModalStateReducer}
import com.serenity.ui.widget.{Loadable, TextField}

/** Before/after for #1926 and #1921, in one JVM so the two sides share a warm-up: a keystroke in a 20k-file picker
  * (reducer step now, synchronous match before) and cursor stepping along a long ASCII line (fast path now, the
  * BreakIterator over a fresh adapter before). Run with `Test / runMain com.serenity.text.PickerAndGraphemeBenchmark`.
  */
object PickerAndGraphemeBenchmark:

  given Balance = Balance.default

  private def millis(rounds: Int)(work: => Unit): Double =
    val start = System.nanoTime()
    (0 until rounds).foreach(_ => work)
    (System.nanoTime() - start) / 1e6 / rounds

  private def twentyThousandFilePicker: AppState =
    val root  = Path.of("/work/project")
    val files = (0 until 20000).map(i => Path.of(s"src/module${i % 97}/pkg$i/Service$i.scala")).toVector
    val picker = FileFinder
      .picker(root)
      .copy(source = Some(PickerSource.ProjectFiles(root, Loadable.Ready(ProjectFileListing(files, false)))))
    ModalStateReducer.show(Modal.ListPicker(picker), AppState.initial).state

  private def pickerReport(): Unit =
    val state             = twentyThousandFilePicker
    val query             = "serv1"
    def keystroke(): Unit = ModalEventReducer.reduce(ModalType.ListPicker, ModalInsertChar('s'), state)
    def matching(): Unit =
      val picker = state.modalSurface.collect {
        case UiSurface(_, SurfaceContent.ModalWorkflow(Modal.ListPicker(p)), _, _) => p
      }
      picker.foreach(p => ListPickerSearch.refreshed(p.copy(query = Some(TextField(query, query.length))), state))
    (0 until 5).foreach { _ =>
      keystroke(); matching()
    }
    (0 until 3).foreach { round =>
      println(
        f"picker round $round: reducer step ${millis(20)(keystroke())}%.3f ms, full match (the old reducer step) ${millis(5)(matching())}%.3f ms"
      )
    }

  private def graphemeReport(): Unit =
    val line  = Rope("The quick brown fox jumps over the lazy dog. " * 2000)
    val steps = 2000
    def now(): Unit =
      (0 until steps).foreach(i => line.nextGraphemeBoundary(i * 40) + line.previousGraphemeBoundary(i * 40 + 5))
    def before(): Unit = (0 until steps).foreach { i =>
      TextEditing.graphemeBreakIterator(RopeCharacterSource(line)).following(i * 40)
      TextEditing.graphemeBreakIterator(RopeCharacterSource(line)).preceding(i * 40 + 5)
    }
    (0 until 5).foreach { _ =>
      now(); before()
    }
    (0 until 3).foreach { round =>
      println(
        f"grapheme round $round: ASCII fast path ${millis(20)(now()) * 1000 / steps}%.3f us/op-pair, BreakIterator over a fresh adapter ${millis(20)(before()) * 1000 / steps}%.3f us/op-pair"
      )
    }

  def main(args: Array[String]): Unit =
    pickerReport()
    graphemeReport()
