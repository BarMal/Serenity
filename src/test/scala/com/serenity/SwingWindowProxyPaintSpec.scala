package com.serenity

import java.awt.image.BufferedImage
import java.awt.{Color, GraphicsEnvironment, Rectangle, Robot, Toolkit}
import javax.swing.{JComponent, JPanel, SwingUtilities}

import scala.concurrent.duration.*

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.config.WindowChromeMode
import com.serenity.testkit.{AwaitCondition, DisplayExclusive}
import com.serenity.ui.accessibility.{
  AccessibilityRole,
  AccessibilitySnapshot,
  AccessibleNode,
  AccessibleValue,
  SwingAccessibilityBridge
}
import com.serenity.ui.layout.LayoutRect
import com.serenity.ui.terminal.SwingWindow
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The canvas presents straight to the window, so the accessibility proxies it carries must not be painted through
  * `RepaintManager` back buffers of their own: a double-buffered child of an unbuffered parent is painted into a shared
  * opaque buffer that nothing initialised, then copied over the frame (JComponent.paint, openjdk/jdk21u).
  */
class SwingWindowProxyPaintSpec extends AnyFlatSpec with Matchers:

  private def onEventThread(action: => Unit): Unit = SwingUtilities.invokeAndWait(() => action)

  private def node(role: AccessibilityRole, bounds: LayoutRect): AccessibleNode =
    AccessibleNode(s"node:${role.toString}", role, role.toString, Some(AccessibleValue.Plain("x")), false, true, bounds)

  private def descendants(component: java.awt.Component): List[java.awt.Component] =
    component match
      case container: java.awt.Container => container.getComponents.toList.flatMap(child => child :: descendants(child))
      case _                             => Nil

  "SwingWindow.presentDirectly" should "keep children added to the canvas afterwards from painting through a buffer" in {
    val canvas = new JPanel
    SwingWindow.presentDirectly(canvas)

    val child  = new JPanel
    val nested = new JPanel
    child.add(nested)
    canvas.add(child)

    List(child, nested).foreach(_.isDoubleBuffered shouldBe false)
  }

  it should "unbuffer children the canvas already had" in {
    val canvas = new JPanel
    val child  = new JPanel
    canvas.add(child)

    SwingWindow.presentDirectly(canvas)

    child.isDoubleBuffered shouldBe false
  }

  "SwingAccessibilityBridge proxies on a directly presented canvas" should "never be double buffered, whatever the role" in {
    val canvas = new JPanel
    SwingWindow.presentDirectly(canvas)
    val bridge = new SwingAccessibilityBridge(canvas)
    val nodes  = AccessibilityRole.values.toList.map(role => node(role, LayoutRect(0, 0, 10, 4)))

    bridge.publish(AccessibilitySnapshot(nodes, Nil))

    canvas.getComponents.length shouldBe nodes.length
    descendants(canvas).collect { case component: JComponent => component }.foreach { component =>
      withClue(s"${component.getClass.getSimpleName}: ")(component.isDoubleBuffered shouldBe false)
    }
  }

  for mode <- List(WindowChromeMode.Native, WindowChromeMode.Custom) do
    s"A shown SwingWindow ($mode chrome) with a dialog proxy over the frame" should "keep showing the frame after a RepaintManager repaint" in {
      assume(!GraphicsEnvironment.isHeadless, "Reading pixels back from the screen needs a display")

      val mismatches = DisplayExclusive(
        SwingWindow
          .resource(chromeMode = mode)
          .use { window =>
            IO.blocking {
              val canvas = window.canvas
              onEventThread(())
              canvas.isShowing shouldBe true

              val width  = canvas.getWidth
              val height = canvas.getHeight
              val frame  = window.acquireBaseImage(width, height, BufferedImage.TYPE_INT_RGB)
              val g      = frame.createGraphics()
              try
                g.setColor(new Color(0x202020))
                g.fillRect(0, 0, width, height)
                g.setColor(new Color(0x40a0ff))
                (0 until height by 6).foreach(y => g.fillRect(0, y, width, 2))
              finally g.dispose()

              val metrics = window.metrics
              val cells   = LayoutRect(2, 2, 20, 6)
              val region = new Rectangle(
                cells.x * metrics.charWidth,
                cells.y * metrics.lineHeight,
                cells.width * metrics.charWidth,
                cells.height * metrics.lineHeight
              )

              onEventThread {
                window.onBaseImageReady(frame)
                canvas.paintImmediately(0, 0, width, height)
              }
              window.updateAccessibility(AccessibilitySnapshot(List(node(AccessibilityRole.Dialog, cells)), Nil))
              onEventThread(())
              onEventThread(canvas.repaint())
              onEventThread(())
              onEventThread(())
              Toolkit.getDefaultToolkit.sync()

              val origin = canvas.getLocationOnScreen
              def mismatchesOnScreen: Int =
                val shot = new Robot().createScreenCapture(
                  new Rectangle(origin.x + region.x, origin.y + region.y, region.width, region.height)
                )
                (for
                  y <- 0 until region.height
                  x <- 0 until region.width
                  if (shot.getRGB(x, y) & 0xffffff) != (frame.getRGB(region.x + x, region.y + y) & 0xffffff)
                yield 1).size
              () => mismatchesOnScreen
            }.flatMap { capture =>
              // The screen shows a frame once the window system has composited it, which on macOS lags behind
              // Toolkit.sync; a stale buffer left by the repaint stays wrong, so waiting cannot hide it.
              AwaitCondition
                .awaitValue(IO.blocking(capture()), 10.seconds)(_ == 0)
                .handleErrorWith(_ => IO.blocking(capture()))
            }
          }
          .unsafeRunSync()
      )

      mismatches shouldBe 0
    }
