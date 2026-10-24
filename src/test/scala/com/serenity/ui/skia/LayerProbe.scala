package com.serenity.ui.skia

import java.awt.{Dimension, GraphicsEnvironment, Rectangle, Robot, Toolkit}
import java.util.concurrent.atomic.AtomicReference
import javax.swing.{JFrame, JPanel, SwingUtilities}

import com.serenity.ui.benchmark.RefreshRate
import org.jetbrains.skia.{Paint, PictureRecorder, Rect}

object LayerProbe:

  private val width  = 400
  private val height = 300

  private def onEdt[A](body: => A): A =
    val result = new AtomicReference[Option[A]](None)
    SwingUtilities.invokeAndWait(() => result.set(Some(body)))
    result.get().getOrElse(sys.error("the event thread produced nothing"))

  def main(args: Array[String]): Unit =
    println(
      s"PROBE os=${sys.props("os.name")} ${sys.props("os.version")} arch=${sys.props("os.arch")} java=${sys.props("java.version")} headless=${GraphicsEnvironment.isHeadless}"
    )
    println(s"PROBE refresh=${RefreshRate.current} toolkit=${Toolkit.getDefaultToolkit.getClass.getName} screen=${Toolkit.getDefaultToolkit.getScreenSize}")
    val canvas = new JPanel(null)
    canvas.setPreferredSize(new Dimension(width, height))
    val frame = onEdt {
      val f = new JFrame("layer probe")
      f.setContentPane(canvas)
      f.pack()
      f.setLocation(0, 0)
      f.setVisible(true)
      f
    }
    val host     = SkiaLayerHost.create(canvas)
    val recorder = new PictureRecorder()
    val drawing  = PictureRecording.begin(recorder, width, height)
    drawing.clear(0xff203040)
    val ink = new Paint()
    ink.setColor(0xffff8000)
    drawing.drawRect(Rect.Companion.makeXYWH(20f, 20f, 100f, 50f), ink)
    val picture = recorder.finishRecordingAsPicture()
    recorder.close()
    host.publish(picture)
    val started = System.nanoTime()
    while host.renderCount == 0 && System.nanoTime() - started < 20_000_000_000L do Thread.sleep(50)
    println(s"PROBE renderApi=${host.renderApi} renders=${host.renderCount} afterMs=${(System.nanoTime() - started) / 1_000_000}")
    val origin = onEdt(canvas.getLocationOnScreen)
    println(s"PROBE canvasOrigin=$origin size=${canvas.getWidth}x${canvas.getHeight}")
    val robot  = new Robot()
    val expect = (0xff8000, 0x203040)
    def sample(): (Int, Int) =
      val shot = robot.createScreenCapture(new Rectangle(origin, new Dimension(width, height)))
      (shot.getRGB(30, 30) & 0xffffff, shot.getRGB(300, 20) & 0xffffff)
    val captureStarted = System.nanoTime()
    var last           = sample()
    while last != expect && System.nanoTime() - captureStarted < 15_000_000_000L do
      Thread.sleep(250)
      last = sample()
    println(f"PROBE capture orange=${last._1}%06x background=${last._2}%06x expected=${expect._1}%06x/${expect._2}%06x matched=${last == expect} afterMs=${(System.nanoTime() - captureStarted) / 1_000_000}")
    println(s"PROBE drawStats=${host.drawStats}")
    host.dispose()
    onEdt(frame.dispose())
    sys.exit(0)
