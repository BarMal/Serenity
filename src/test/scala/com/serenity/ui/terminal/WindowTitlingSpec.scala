package com.serenity.ui.terminal

import scala.collection.mutable.ListBuffer

import cats.effect.IO
import com.serenity.config.PreferredWindowSize
import com.serenity.state.models.PointerShape
import com.serenity.ui.accessibility.AccessibilitySnapshot
import com.serenity.ui.layout.{CellMetrics, ViewportSize}
import com.serenity.ui.theme.Theme
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class WindowTitlingSpec extends AnyFlatSpec with Matchers:

  private class Probe extends WindowTitling:
    val applied  = ListBuffer.empty[String]
    val onThread = ListBuffer.empty[String]

    protected def applyTitle(title: String): Unit = applied += title

    override protected def onEventThread(run: Runnable): Unit =
      onThread += "ran"
      run.run()

    def awaitClose: IO[Unit]                                                  = IO.never
    def setOnResize(cb: () => Unit): Unit                                     = ()
    def setOnFocusChange(cb: Boolean => Unit): Unit                           = ()
    def doResizeIfNecessary(): Option[ViewportSize]                           = None
    def hide(): Unit                                                          = ()
    def bringToFront(): Unit                                                  = ()
    def currentPreferredWindowSize: PreferredWindowSize                       = PreferredWindowSize.Default
    def resizeToPreferred(size: PreferredWindowSize): Unit                    = ()
    def updateMetrics(codeMetrics: CellMetrics, uiMetrics: CellMetrics): Unit = ()
    def updateChromeTheme(theme: Theme): Unit                                 = ()
    def updatePointerShape(shape: PointerShape): Unit                         = ()
    def updateAccessibility(snapshot: AccessibilitySnapshot): Unit            = ()
    def endInputMethodComposition(): Unit                                     = ()
    def detectedDeviceTextScale: Double                                       = 1.0

  "A titled window" should "apply a title once however often it is offered, and again when it changes" in {
    val window = new Probe

    List("a.md — Serenity", "a.md — Serenity", "a.md* — Serenity", "a.md* — Serenity", "a.md — Serenity")
      .foreach(window.setTitle)

    window.applied.toList shouldBe List("a.md — Serenity", "a.md* — Serenity", "a.md — Serenity")
    window.onThread.size shouldBe 3
  }
