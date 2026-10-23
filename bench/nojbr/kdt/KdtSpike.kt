package org.jetbrains.desktop.sample.linux

import org.jetbrains.desktop.linux.Application
import org.jetbrains.desktop.linux.ApplicationConfig
import org.jetbrains.desktop.linux.DataSource
import org.jetbrains.desktop.linux.DragAndDropQueryResponse
import org.jetbrains.desktop.linux.Event
import org.jetbrains.desktop.linux.EventHandlerResult
import org.jetbrains.desktop.linux.KotlinDesktopToolkit
import org.jetbrains.desktop.linux.LogicalPixelsInt
import org.jetbrains.desktop.linux.LogicalRect
import org.jetbrains.desktop.linux.LogicalSize
import org.jetbrains.desktop.linux.PointerShape
import org.jetbrains.desktop.linux.RenderingMode
import org.jetbrains.desktop.linux.TextInputContentPurpose
import org.jetbrains.desktop.linux.TextInputContext
import org.jetbrains.desktop.linux.Window
import org.jetbrains.desktop.linux.WindowFrame
import org.jetbrains.desktop.linux.WindowParams
import java.awt.Color
import java.awt.Font
import java.awt.RenderingHints
import java.awt.image.BufferedImage
import java.awt.image.DataBufferInt
import java.lang.foreign.MemorySegment
import java.nio.file.Files
import java.nio.file.Path
import java.util.Arrays
import java.util.Locale
import kotlin.concurrent.thread

// Throwaway spike for #2380 (never merge): the same gates as the SDL3 spike, against kotlin-desktop-toolkit.
private val t0 = System.nanoTime()
private val out: Path = Path.of(System.getProperty("spike.out") ?: "/tmp/kdt-spike")
private val imeMode = System.getProperty("spike.mode") == "ime"

private fun say(s: String) {
    println(s)
    System.out.flush()
}

private fun ev(s: String) = say(String.format(Locale.ROOT, "EV %8.1fms %s", (System.nanoTime() - t0) / 1e6, s))

private fun go(k: Int) = Files.exists(out.resolve("go.$k"))

private fun cps(t: String?): String = t?.codePoints()?.toArray()?.joinToString(" ", "[", "]") { Integer.toHexString(it) } ?: "[]"

private fun pct(sorted: LongArray, p: Double): Double = sorted[minOf(sorted.size - 1, (p * sorted.size).toInt())] / 1e6

private class Spike(val app: Application) {
    val windowId = 1L
    var window: Window? = null
    var img = BufferedImage(1, 1, BufferedImage.TYPE_INT_ARGB)
    var state = IntArray(1)
    var pw = 0
    var ph = 0
    var phase = 0
    var announced = 0
    var scale = 1.0
    var drawCount = 0L
    var lastDraw = 0L
    var idleStart = 0L
    var idleDraws = 0L
    val copyNs = LongArray(230)
    val gapNs = LongArray(230)
    var tN = 0
    var textInputOn = false
    var surfaceSaid = false
    val rw = 240
    val rh = 160
    var clipGot = "none"
    var primaryGot = "none"
    @Volatile var stopRequested = false

    fun resize(w: Int, h: Int) {
        pw = w
        ph = h
        img = BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB)
        state = (img.raster.dataBuffer as DataBufferInt).data
        Arrays.fill(state, 0xFF202030.toInt())
        if (phase >= 1) Arrays.fill(state, 0xFFC81E1E.toInt())
    }

    fun fill(x: Int, y: Int, w: Int, h: Int, argb: Int) {
        for (yy in y until minOf(y + h, ph)) Arrays.fill(state, yy * pw + x, minOf(yy * pw + x + w, yy * pw + pw), argb)
    }

    fun textFrame(seed: Int) {
        val g = img.createGraphics()
        try {
            g.color = Color(0x1e1e24)
            g.fillRect(0, 0, pw, ph)
            g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
            g.setRenderingHint(RenderingHints.KEY_FRACTIONALMETRICS, RenderingHints.VALUE_FRACTIONALMETRICS_ON)
            g.color = Color(0xdddddd)
            g.font = Font(Font.MONOSPACED, Font.PLAIN, Math.round(14 * maxOf(1.0, scale)).toInt())
            val lh = g.fontMetrics.height
            var row = 0
            var y = lh
            while (y < ph) {
                g.drawString("line ${row + seed} the quick brown fox jumps over the lazy dog 0123456789 {}[]()", 8, y)
                row++
                y += lh
            }
        } finally {
            g.dispose()
        }
    }

    fun announce(k: String, colour: String, expect: Long) {
        say("PHASE $k $colour expect=$expect")
    }

    fun onDraw(e: Event.WindowDraw) {
        val sd = e.softwareDrawData
        if (sd == null) {
            say("WARN draw without software data (EGL?)")
            return
        }
        val w = e.size.width.rawPhysical
        val h = e.size.height.rawPhysical
        if (w != pw || h != ph) resize(w, h)
        val now = System.nanoTime()
        drawCount++
        if (!surfaceSaid) {
            surfaceSaid = true
            say("SURFACE ${w}x$h stride=${sd.stride} drawsFrom=KDT software buffer scale=$scale")
        }
        // Phase machine, advanced by the driver script touching go.N
        if (imeMode) {
            if (phase == 0) { phase = 99; say("PHASE ime-ready") }
        } else when {
            phase == 0 -> { phase = 1; Arrays.fill(state, 0xFFC81E1E.toInt()) }
            phase == 1 && announced < 1 -> { announced = 1; announce("1", "200,30,30", w.toLong() * h) }
            phase == 1 && go(1) -> { phase = 2; fill(100, 100, rw, rh, 0xFF1EC81E.toInt()) }
            phase == 2 && announced < 2 -> { announced = 2; announce("2", "30,200,30", rw.toLong() * rh) }
            phase == 2 && go(2) -> { phase = 3; fill(400, 300, rw, rh, 0xFF1E1EC8.toInt()) }
            phase == 3 && announced < 3 -> { announced = 3; announce("3", "30,30,200", rw.toLong() * rh) }
            phase == 3 && go(3) -> { phase = 4; Arrays.fill(state, 0xFFC8C81E.toInt()) }
            phase == 4 && announced < 4 -> { announced = 4; announce("4", "200,200,30", w.toLong() * h); idleStart = now; idleDraws = 0 }
            phase == 4 && go(4) -> {
                val secs = (now - idleStart) / 1e9
                say(String.format(Locale.ROOT, "IDLE draws_per_s=%.1f over %.1fs (state unchanged)", idleDraws / secs, secs))
                phase = 5
            }
            phase == 4 -> idleDraws++
            phase == 5 -> {
                // timing: render the text frame (excluded), copy into the toolkit's buffer (timed)
                textFrame(tN)
                if (lastDraw != 0L && tN > 0) gapNs[tN - 1] = now - lastDraw
                val c0 = System.nanoTime()
                copyTo(sd.canvas, sd.stride, w, h)
                val c1 = System.nanoTime()
                if (tN < copyNs.size) copyNs[tN] = c1 - c0
                tN++
                lastDraw = now
                if (tN >= copyNs.size) {
                    val warm = 30
                    val c = copyNs.copyOfRange(warm, copyNs.size).also { it.sort() }
                    val g = gapNs.copyOfRange(warm, gapNs.size - 1).also { it.sort() }
                    say(
                        String.format(
                            Locale.ROOT,
                            "TIMING full-copy-into-kdt-buffer %dx%d copy_p50=%.3fms copy_p95=%.3fms frame_gap_p50=%.3fms frame_gap_p95=%.3fms",
                            w, h, pct(c, .5), pct(c, .95), pct(g, .5), pct(g, .95),
                        ),
                    )
                    say("COEXIST headless=${java.awt.GraphicsEnvironment.isHeadless()} java2d-text-ok")
                    phase = 6
                    say("PHASE input-ready")
                }
                return
            }
        }
        copyTo(sd.canvas, sd.stride, w, h)
    }

    fun copyTo(addr: Long, stride: Int, w: Int, h: Int) {
        val seg = MemorySegment.ofAddress(addr).reinterpret(stride.toLong() * h)
        val src = MemorySegment.ofArray(state)
        if (stride == w * 4) {
            MemorySegment.copy(src, 0, seg, 0, w.toLong() * h * 4)
        } else {
            for (y in 0 until h) MemorySegment.copy(src, y.toLong() * w * 4, seg, y.toLong() * stride, w * 4L)
        }
    }

    fun onEvent(e: Event): EventHandlerResult {
        when (e) {
            is Event.ApplicationStarted -> {
                ev("application_started")
                window = app.createWindow(
                    WindowParams(
                        windowId = windowId,
                        appId = "org.serenity.kdtspike",
                        title = "kdt-spike",
                        size = LogicalSize.makeWH(800, 600),
                        renderingMode = RenderingMode.Software,
                        clientSideDecorationFrame = WindowFrame(
                            padding = WindowFrame.Padding.withAll(LogicalPixelsInt(0)),
                            resizerThickness = WindowFrame.ResizerThickness.withAll(LogicalPixelsInt(8)),
                        ),
                    ),
                )
                return EventHandlerResult.Stop
            }
            is Event.WindowDraw -> {
                onDraw(e)
                return EventHandlerResult.Stop // upstream's sample returns Stop once the frame is drawn; Continue leaves it uncommitted
            }
            is Event.WindowConfigure -> {
                ev("window_configure size=${e.size.width.rawLogical}x${e.size.height.rawLogical} active=${e.active} decoration=${e.decorationMode}")
                say("SCALE logical=${e.size.width.rawLogical}x${e.size.height.rawLogical} scaleSoFar=$scale decoration=${e.decorationMode}")
            }
            is Event.WindowScaleChanged -> {
                scale = e.newScale.rawScale
                ev("window_scale_changed scale=$scale")
                say("SCALE preferred=$scale")
            }
            is Event.KeyDown -> ev("key down code=${e.keyCode.value} sym=0x${e.key.value.toString(16)} chars='${e.characters}' cps=${cps(e.characters)}")
            is Event.KeyUp -> ev("key up code=${e.keyCode.value} sym=0x${e.key.value.toString(16)}")
            is Event.ModifiersChanged -> ev("modifiers ${e.modifiers}")
            is Event.MouseDown -> ev("mouse_button down=true button=${e.button}")
            is Event.MouseUp -> ev("mouse_button down=false button=${e.button}")
            is Event.MouseMoved -> ev("mouse_motion x=${e.locationInWindow.x.rawLogical} y=${e.locationInWindow.y.rawLogical}")
            is Event.MouseEntered -> ev("window_mouse_enter")
            is Event.ScrollWheel -> ev("mouse_wheel v120=${e.verticalScroll.wheelValue120} delta=${e.verticalScroll.delta.rawLogical}")
            is Event.WindowKeyboardEnter -> ev("window_focus_gained")
            is Event.WindowKeyboardLeave -> ev("window_focus_lost")
            is Event.TextInputAvailability -> {
                ev("text_input_availability available=${e.available}")
                if (e.available && !textInputOn) {
                    textInputOn = true
                    app.textInputEnable(
                        TextInputContext(
                            surroundingText = "",
                            cursorCodepointOffset = 0u,
                            selectionStartCodepointOffset = 0u,
                            hints = emptySet(),
                            contentPurpose = TextInputContentPurpose.Normal,
                            cursorRectangle = LogicalRect.makeXYWH(100, 100, 50, 20),
                            changeCausedByInputMethod = false,
                        ),
                    )
                }
            }
            is Event.TextInput -> {
                val p = e.preeditStringData
                val c = e.commitStringData
                if (p != null) ev("text_editing '${p.text}' cps=${cps(p.text)} cursorBeginByte=${p.cursorBeginBytePos} cursorEndByte=${p.cursorEndBytePos}")
                if (c != null) ev("text_input '${c.text}' cps=${cps(c.text)}")
                if (e.deleteSurroundingTextData != null) ev("text_delete_surrounding ${e.deleteSurroundingTextData}")
            }
            is Event.DataTransfer -> {
                val txt = e.content?.data?.toString(Charsets.UTF_8)
                ev("data_transfer serial=${e.serial} mime=${e.content?.mimeType} text='$txt'")
                if (e.serial == 1) clipGot = "'$txt'" else if (e.serial == 2) primaryGot = "'$txt'"
            }
            is Event.DataTransferAvailable -> ev("data_transfer_available $e")
            is Event.WindowCloseRequest -> return EventHandlerResult.Stop
            else -> {}
        }
        return if (stopRequested) EventHandlerResult.Stop else EventHandlerResult.Continue
    }

    /** Runs on the event loop thread; polls the driver script's go.N files for the steps that are not draw-driven. */
    fun pollSteps() {
        if (imeMode) {
            if (go(99)) { stopRequested = true; app.stopEventLoop() }
            return
        }
        if (phase == 6 && go(5)) {
            phase = 7
            val win = window ?: return
            for (shape in listOf(PointerShape.Text, PointerShape.Pointer, PointerShape.EwResize)) {
                win.setPointerShape(shape)
                Thread.sleep(300)
            }
            say("CURSOR set text,pointer,ew-resize")
            say("PHASE input-done")
        }
        if (phase == 7 && go(6)) {
            phase = 8
            say("PHASE clip-ready")
        }
        if (phase == 8 && go(7)) {
            phase = 9
            app.clipboardPaste(1, listOf("text/plain;charset=utf-8", "text/plain"))
            app.primarySelectionPaste(2, listOf("text/plain;charset=utf-8", "text/plain"))
            thread(isDaemon = true) {
                Thread.sleep(1500)
                app.runOnEventLoopAsync {
                    say("CLIP get clipboard=$clipGot primary=$primaryGot")
                    app.clipboardPut(listOf("text/plain;charset=utf-8"))
                    app.primarySelectionPut(listOf("text/plain;charset=utf-8"))
                    say("CLIP set clipboard=true primary=true")
                    say("PHASE clip-set")
                }
            }
        }
        if (phase == 9 && go(8)) {
            say("DONE")
            stopRequested = true
            app.stopEventLoop()
        }
    }
}

fun main() {
    Files.createDirectories(out)
    say("JAVA ${System.getProperty("java.version")} ${System.getProperty("java.vendor")}")
    KotlinDesktopToolkit.init()
    Application().use { app ->
        val spike = Spike(app)
        thread(isDaemon = true) {
            while (true) {
                Thread.sleep(100)
                try {
                    app.runOnEventLoopAsync { spike.pollSteps() }
                } catch (_: Throwable) {
                    return@thread
                }
            }
        }
        app.runEventLoop(
            ApplicationConfig(
                eventHandler = spike::onEvent,
                queryDragAndDropTarget = { DragAndDropQueryResponse(emptyList()) },
                getDataTransferData = { source: DataSource, mime: String ->
                    if (mime.startsWith("text/plain")) {
                        (if (source == DataSource.PrimarySelection) "from-kdt-primary" else "from-kdt-clipboard").toByteArray()
                    } else {
                        null
                    }
                },
            ),
        )
    }
    say("DONE (event loop exited)")
}
