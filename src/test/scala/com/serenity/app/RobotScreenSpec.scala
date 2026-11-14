package com.serenity.app

import java.awt.{Color, Dimension, Graphics, GraphicsEnvironment}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import javax.swing.{JFrame, JPanel, SwingUtilities}

import scala.concurrent.duration.*

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.app.PixelClock.Region
import com.serenity.testkit.DisplayExclusive
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The clock against a real display: Xvfb on Linux (the Test job has one). */
class RobotScreenSpec extends AnyFlatSpec with Matchers:

  private def onEdt[A](body: => A): A =
    val result = new AtomicReference[Option[A]](None)
    SwingUtilities.invokeAndWait(() => result.set(Some(body)))
    result.get().getOrElse(fail("the event thread produced nothing"))

  private def awaitPainted(painted: AtomicBoolean): Unit =
    def poll: IO[Unit] = IO.defer(if painted.get then IO.unit else IO.sleep(20.millis) *> poll)
    poll.timeout(10.seconds).unsafeRunSync()

  private def withPanel[A](body: (JPanel, Region) => A): A =
    assume(!GraphicsEnvironment.isHeadless, "reading the screen needs a display")
    DisplayExclusive:
      val painted = new AtomicBoolean(false)
      val panel = new JPanel(null):
        override def paintComponent(g: Graphics): Unit =
          super.paintComponent(g)
          painted.set(true)
      panel.setBackground(Color.BLACK)
      panel.setPreferredSize(new Dimension(120, 80))
      val frame = onEdt {
        val f = new JFrame("pixel clock")
        f.setContentPane(panel)
        f.pack()
        f.setLocation(0, 0)
        f.setVisible(true)
        f
      }
      try
        awaitPainted(painted)
        val origin = onEdt(panel.getLocationOnScreen)
        body(panel, Region(origin.x + 10, origin.y + 10, 16, 8))
      finally onEdt(frame.dispose())

  "RobotScreen" should "read the pixels of a region of the screen" in
    withPanel { (_, region) =>
      val pixels = RobotScreen().flatMap(_.grab(region)).unsafeRunSync()
      pixels should have size (16 * 8)
      pixels.distinct shouldBe Vector(Color.BLACK.getRGB)
    }

  "PixelClock on a real screen" should "see a repaint and answer how long it took" in
    withPanel { (panel, region) =>
      val elapsed = RobotScreen()
        .flatMap { screen =>
          PixelClock.timeChange(
            screen,
            region,
            IO(SwingUtilities.invokeLater { () =>
              panel.setBackground(Color.WHITE)
              panel.repaint()
            }),
            5.seconds
          )
        }
        .unsafeRunSync()
      elapsed.exists(ms => ms > 0.0 && ms < 5000.0) shouldBe true
    }

  it should "answer none when nothing on the screen changes" in
    withPanel { (_, region) =>
      RobotScreen()
        .flatMap(screen => PixelClock.timeChange(screen, region, IO.unit, 300.millis))
        .unsafeRunSync() shouldBe None
    }

  it should "measure a resolution that is a real, finite grab time" in
    withPanel { (_, region) =>
      val resolution =
        RobotScreen().flatMap(screen => PixelClock.resolution(screen, region, samples = 20)).unsafeRunSync()
      resolution should (be > 0.0 and be < 1000.0)
    }
