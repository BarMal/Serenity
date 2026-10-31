package com.serenity.perf

import java.awt.Font

import com.serenity.config.{AppConfig, TextAreaInsets}
import com.serenity.rope.Balance
import com.serenity.state.manager.RenderCaches
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.{CellMetrics, ViewportSize, WorkspaceNode, WorkspaceNodeId, WorkspaceTree}
import com.serenity.ui.theme.Theme

/** How long a cold scene build (`AuthoritativeUiScene.forState`, the "layout" of a cold frame) takes with the scrollbar
  * on and off, on a 1200-line document of prose and code like the one `--render-benchmark` uses. Every call gets fresh
  * caches. The two arms run interleaved in blocks, ON OFF OFF ON, in one process, after unmeasured warm-up calls.
  *
  * This times the scene build only, not a whole frame: nothing is drawn. Usage:
  * `java -cp serenity-perf.jar com.serenity.perf.ColdLayoutScrollbarProbe [calls per block] [warm-up calls]`.
  */
object ColdLayoutScrollbarProbe:

  private val bufferId = BufferId(1)
  private val paneId   = PaneId(0)

  private val lines: Vector[String] =
    Vector.tabulate(1200) { index =>
      if index % 36 < 12 then s"Paragraph $index " + "lorem ipsum dolor sit amet " * (4 + index % 5 * 4)
      else s"  def step$index(value: Int): Int = value * ${index % 7 + 2} + ${index % 13}"
    }

  private def document(scrollbar: Boolean)(using Balance): AppState =
    val base = Buffer.fromString(bufferId, lines.mkString("\n"))
    val buffer = base.copy(
      document = base.document.copy(isDirty = true, language = None),
      viewport = Viewport(topLine = 0, leftColumn = 0, visibleLines = 40, visibleColumns = 120),
      editing = EditingState(List(CursorPosition(4, 0)))
    )
    val state  = AppState.initial(AppConfig.conventional)
    val config = AppConfig.conventional.withoutStatusLine
    state.copy(persisted =
      state.persisted.copy(
        buffers = Map(buffer.id -> buffer),
        bufferOrder = List(buffer.id),
        layout = state.persisted.layout.copy(
          editorPanes = Map(paneId -> EditorPane.withBuffer(paneId, buffer.id)),
          activeEditorPaneId = Some(paneId),
          workspaceTree = Some(WorkspaceTree(WorkspaceNode.Leaf(WorkspaceNodeId(s"editor-${paneId.value}"), paneId)))
        ),
        focus = Focus.EditorPane(paneId),
        theme = Theme.light,
        config = config
          .copy(surfaceConfig = config.surfaceConfig.copy(textAreaInsets = TextAreaInsets(0.04, 0.04, 0.04, 0.04)))
          .withScrollbar(scrollbar)
      )
    )

  private def coldScene(state: AppState, viewport: ViewportSize, code: Font, text: Font): Double =
    val caches  = RenderCaches.create()
    val started = System.nanoTime()
    val scene   = caches.authoritativeScene.forState(state, viewport, code, text)
    val millis  = (System.nanoTime() - started) / 1e6
    require(scene.paneLayouts.nonEmpty, "the scene has no pane")
    millis

  private def percentile(sorted: Vector[Double], q: Double): Double = sorted(((sorted.size - 1) * q).toInt)

  private def summary(label: String, samples: Vector[Double]): String =
    val sorted = samples.sorted
    f"$label%-14s n=${sorted.size}%4d  p50 ${percentile(sorted, 0.5)}%7.2f ms  p95 ${percentile(sorted, 0.95)}%7.2f ms  max ${sorted.last}%7.2f ms"

  def main(args: Array[String]): Unit =
    given Balance = Balance.default
    val perBlock  = args.lift(0).flatMap(_.toIntOption).getOrElse(50)
    val warmUp    = args.lift(1).flatMap(_.toIntOption).getOrElse(30)
    val fonts     = FontLoader.FontConfig()
    val code      = FontLoader.previewCodeFont(fonts)
    val text      = FontLoader.previewTextFont(fonts)
    val metrics   = CellMetrics.fromFont(code)
    val viewport  = ViewportSize(1600 / metrics.charWidth, 1000 / metrics.lineHeight)
    val on        = document(scrollbar = true)
    val off       = document(scrollbar = false)
    println(s"Cold scene build (forState), 1600x1000 -> viewport $viewport, 1200-line document, fresh caches per call.")
    println(
      s"java ${System.getProperty("java.version")}, ${java.lang.Runtime.getRuntime.availableProcessors} processors, ${System.getProperty("os.name")} ${System.getProperty("os.arch")}"
    )
    (0 until warmUp).foreach { _ =>
      coldScene(on, viewport, code, text)
      coldScene(off, viewport, code, text)
    }
    val blocks = List("A1 ON" -> on, "B1 OFF" -> off, "B2 OFF" -> off, "A2 ON" -> on)
    val measured = blocks.map {
      case (label, state) =>
        val samples = Vector.fill(perBlock)(coldScene(state, viewport, code, text))
        println(summary(label, samples))
        label -> samples
    }
    def pooled(prefix: String) = measured.filter(_._1.endsWith(prefix)).flatMap(_._2).toVector
    println(summary("scrollbar ON", pooled("ON")))
    println(summary("scrollbar OFF", pooled("OFF")))
