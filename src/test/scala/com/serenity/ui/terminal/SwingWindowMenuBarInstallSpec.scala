package com.serenity.ui.terminal

import java.awt.{Component, Container, GraphicsEnvironment}
import javax.swing.{JMenu, JMenuBar, SwingUtilities}

import com.serenity.config.WindowChromeMode
import com.serenity.testkit.DisplayExclusive
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Needs a display: the window under test is a real `JFrame`. */
class SwingWindowMenuBarInstallSpec extends AnyFlatSpec with Matchers:

  private def barNamed(title: String): JMenuBar =
    val bar = new JMenuBar
    val _   = bar.add(new JMenu(title))
    bar

  private def barsIn(component: Component): List[JMenuBar] =
    component match
      case bar: JMenuBar => List(bar)
      case container: Container =>
        container.getComponents.toList.flatMap(barsIn) ++
          (container match
            case frame: javax.swing.JFrame => Option(frame.getJMenuBar).toList
            case _                         => Nil)
      case _ => Nil

  private def withWindow(chrome: WindowChromeMode)(body: (SwingWindow, () => List[JMenuBar]) => Unit): Unit =
    assume(!GraphicsEnvironment.isHeadless, "A menu bar is installed in a real frame, which needs a display")
    DisplayExclusive:
      val window = new SwingWindow(
        new java.awt.Dimension(640, 480),
        com.serenity.ui.layout.CellMetrics(8, 16, 13),
        chrome,
        com.serenity.ui.layout.CellMetrics(8, 16, 13),
        env = Map("XDG_SESSION_TYPE" -> "x11")
      )
      try
        val frame = SwingUtilities.getWindowAncestor(window.canvas)
        body(window, () => barsIn(frame))
      finally
        window.stop()
        // A repaint still queued for this window would reach the next display spec's RepaintManager count.
        SwingUtilities.invokeAndWait(() => ())
        SwingUtilities.invokeAndWait(() => ())

  "Installing a menu bar" should "leave exactly one bar under the custom title bar, whichever is installed last" in
    withWindow(WindowChromeMode.Custom): (window, bars) =>
      val first  = barNamed("First")
      val second = barNamed("Second")

      window.installMenuBar(first)
      window.installMenuBar(second)

      bars() should contain only second
      window.isMenuBarInstalled shouldBe true

  it should "leave exactly one bar when the same bar is installed again" in
    withWindow(WindowChromeMode.Custom): (window, bars) =>
      val bar = barNamed("Only")

      window.installMenuBar(bar)
      window.installMenuBar(bar)

      bars() should contain only bar

  it should "leave exactly one bar in a natively decorated frame" in
    withWindow(WindowChromeMode.Native): (window, bars) =>
      val first  = barNamed("First")
      val second = barNamed("Second")

      window.installMenuBar(first)
      window.installMenuBar(second)

      bars() should contain only second

  "Removing the menu bar" should "leave none, and installing it again brings it back" in
    withWindow(WindowChromeMode.Custom): (window, bars) =>
      val bar = barNamed("Bar")
      window.installMenuBar(bar)

      window.removeMenuBar()
      bars() shouldBe Nil
      window.isMenuBarInstalled shouldBe false

      window.installMenuBar(bar)
      bars() should contain only bar
