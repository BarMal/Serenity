package com.serenity.perf

import java.awt.Font
import java.util.concurrent.{Executors, Semaphore, TimeUnit}

import scala.annotation.tailrec

import com.serenity.config.{AppConfig, TextAreaInsets}
import com.serenity.rope.Balance
import com.serenity.state.manager.{AuthoritativeUiScene, RenderCaches, ScrollCounting}
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.{CellMetrics, ViewportSize, WorkspaceNode, WorkspaceNodeId, WorkspaceTree}
import com.serenity.ui.theme.Theme

/** What counting a pane's rows late (#2445) does to a cold scene build, on a 1200-line document of prose and code like
  * the one `--render-benchmark` uses. Every call gets fresh caches. Two arms run interleaved in blocks, TODAY LATE LATE
  * TODAY, in one process, after unmeasured warm-up calls:
  *   - TODAY: the scene build with the whole document counted inside it (what the app does on master);
  *   - LATE first scene: the scene build with only the lines on screen counted, which is what a frame waits for;
  *   - LATE time to bar: from the start of that scene build until the scene carries the scrollbar, counted on a
  *     background thread.
  *
  * This times the scene build only, not a whole frame: nothing is drawn. Usage:
  * `java -cp serenity-perf.jar com.serenity.perf.LateScrollbarProbe [calls per block] [warm-up calls]`.
  */
object LateScrollbarProbe:

  private val bufferId = BufferId(1)
  private val paneId   = PaneId(0)

  private val lines: Vector[String] =
    Vector.tabulate(1200) { index =>
      if index % 36 < 12 then s"Paragraph $index " + "lorem ipsum dolor sit amet " * (4 + index % 5 * 4)
      else s"  def step$index(value: Int): Int = value * ${index % 7 + 2} + ${index % 13}"
    }

  private def document(using Balance): AppState =
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
        config = config.copy(surfaceConfig =
          config.surfaceConfig.copy(textAreaInsets = TextAreaInsets(0.04, 0.04, 0.04, 0.04))
        )
      )
    )

  final private case class Sample(sceneMs: Double, barMs: Double)

  private def millisSince(started: Long): Double = (System.nanoTime() - started) / 1e6

  private def today(state: AppState, viewport: ViewportSize, code: Font, text: Font): Sample =
    val caches  = RenderCaches.create()
    val started = System.nanoTime()
    val scene   = caches.authoritativeScene.forState(state, viewport, code, text)
    val ms      = millisSince(started)
    require(scene.paneScrolls.contains(paneId), "today's scene has no bar")
    Sample(ms, ms)

  private def late(state: AppState, viewport: ViewportSize, code: Font, text: Font): Sample =
    val executor = Executors.newSingleThreadExecutor { runnable =>
      val thread = new Thread(runnable, "late-scrollbar-probe")
      thread.setDaemon(true)
      thread
    }
    val counts  = new Semaphore(0)
    val scene   = AuthoritativeUiScene(scrollCounting = ScrollCounting.deferred(executor, _ => counts.release()))
    val started = System.nanoTime()
    val first   = scene.forState(state, viewport, code, text)
    val sceneMs = millisSince(started)
    require(!first.paneScrolls.contains(paneId), "the first late scene already has its bar")
    @tailrec def untilBar(): Unit =
      if !scene.forState(state, viewport, code, text).paneScrolls.contains(paneId) then
        require(counts.tryAcquire(30, TimeUnit.SECONDS), "the bar never landed")
        untilBar()
    untilBar()
    val barMs = millisSince(started)
    executor.shutdown()
    Sample(sceneMs, barMs)

  private def percentile(sorted: Vector[Double], q: Double): Double = sorted(((sorted.size - 1) * q).toInt)

  private def line(label: String, name: String, values: Vector[Double]): String =
    val sorted = values.sorted
    f"$label%-10s $name%-14s n=${sorted.size}%4d  p50 ${percentile(sorted, 0.5)}%7.2f ms  p95 ${percentile(sorted, 0.95)}%7.2f ms  max ${sorted.last}%7.2f ms"

  def main(args: Array[String]): Unit =
    given Balance = Balance.default
    val perBlock  = args.lift(0).flatMap(_.toIntOption).getOrElse(50)
    val warmUp    = args.lift(1).flatMap(_.toIntOption).getOrElse(30)
    val fonts     = FontLoader.FontConfig()
    val code      = FontLoader.previewCodeFont(fonts)
    val textFont  = FontLoader.previewTextFont(fonts)
    val metrics   = CellMetrics.fromFont(code)
    val viewport  = ViewportSize(1600 / metrics.charWidth, 1000 / metrics.lineHeight)
    val state     = document
    println(s"Cold scene build (forState), 1600x1000 -> viewport $viewport, 1200-line document, fresh caches per call.")
    println(
      s"java ${System.getProperty("java.version")}, ${java.lang.Runtime.getRuntime.availableProcessors} processors, ${System.getProperty("os.name")} ${System.getProperty("os.arch")}"
    )
    (0 until warmUp).foreach { _ =>
      today(state, viewport, code, textFont)
      late(state, viewport, code, textFont)
    }
    val blocks = List("A1 TODAY" -> true, "B1 LATE" -> false, "B2 LATE" -> false, "A2 TODAY" -> true)
    val measured = blocks.map {
      case (label, isToday) =>
        val samples = Vector.fill(perBlock)(
          if isToday then today(state, viewport, code, textFont) else late(state, viewport, code, textFont)
        )
        if isToday then println(line(label, "scene build", samples.map(_.sceneMs)))
        else
          println(line(label, "first scene", samples.map(_.sceneMs)))
          println(line(label, "time to bar", samples.map(_.barMs)))
        label -> samples
    }
    def pooled(suffix: String) = measured.filter(_._1.endsWith(suffix)).flatMap(_._2).toVector
    println(line("TODAY", "scene build", pooled("TODAY").map(_.sceneMs)))
    println(line("LATE", "first scene", pooled("LATE").map(_.sceneMs)))
    println(line("LATE", "time to bar", pooled("LATE").map(_.barMs)))
