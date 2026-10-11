package com.serenity.ui.tui

import java.nio.charset.StandardCharsets

import cats.effect.IO
import com.serenity.diagnostics.FrameCounter

/** The bytes the frame counters say the TUI sent, checked against the bytes the terminal actually received. */
class TuiFrameCountersSpec extends TuiSpec:

  private val frameCounters = session.map(_.stateManager.renderCaches.frameCounters)

  private def utf8Length(text: String): Long = text.getBytes(StandardCharsets.UTF_8).length.toLong

  "the TUI bytes counter" should "total exactly what a typing frame and the frame after it sent" in
    runTui(TuiEnvironment.withFile("abc")) {
      for
        counters <- frameCounters
        _        <- settledScreen
        _        <- lineEnd
        _        <- settledScreen
        _        <- liftIO(IO(counters.setEnabled(true)))
        _        <- typeText("d")
        typed    <- screen
        quiet    <- screen
      yield
        val window = counters.drain()
        window.frames shouldBe 2
        window.totals(FrameCounter.TuiBytes) shouldBe utf8Length(typed.emitted) + utf8Length(quiet.emitted)
        window.totals(FrameCounter.TuiBytes) should be > 0L
        window.totals(FrameCounter.TuiBytes) should be < 2000L
    }

  it should "be zero for a frame that changed nothing" in
    runTui(TuiEnvironment.withFile("unchanging")) {
      for
        counters <- frameCounters
        _        <- settledScreen
        _        <- liftIO(IO(counters.setEnabled(true)))
        _        <- screen
        _        <- screen
        again    <- screen
      yield
        again.emitted shouldBe ""
        counters.lastFrame(FrameCounter.TuiBytes) shouldBe 0L
    }

  it should "stay at zero while the counters are off, whatever the terminal is sent" in
    runTui(TuiEnvironment.withFile("abc")) {
      for
        counters <- frameCounters
        _        <- settledScreen
        _        <- typeText("d")
        typed    <- screen
      yield
        typed.emitted should not be empty
        counters.lastFrame(FrameCounter.TuiBytes) shouldBe 0L
    }
