package com.serenity.ui.tui

import java.awt.event.{WindowAdapter, WindowEvent}
import java.awt.image.BufferedImage
import java.awt.{BorderLayout, Dimension, Graphics}
import java.util.concurrent.atomic.AtomicReference
import javax.swing.{JFrame, JPanel, SwingUtilities, WindowConstants}

import cats.effect.{IO, Resource}

/** A minimal, display-only preview window for the active Markdown buffer, spawned side-by-side from TUI mode (issue
  * #1113). Deliberately a plain `JFrame` + image panel rather than [[com.serenity.ui.terminal.SwingWindow]] -- there is
  * no editing surface, no custom chrome, and no input handling; it only ever paints whatever `BufferedImage`
  * [[com.serenity.markdown.MarkdownDocumentPreview]] last rendered for the followed buffer. The window never requests
  * keyboard focus, so it never steals it from the terminal.
  */
trait MarkdownPreviewWindow:
  def show(): IO[Unit]
  def hide(): IO[Unit]
  def updateImage(image: BufferedImage): IO[Unit]

  /** The image panel's current pixel size, read live off the Swing component so the next rendered image matches
    * whatever size the user has resized the window to.
    */
  def currentSize: IO[(Int, Int)]

  /** Registers the callback invoked when the user closes the window via its native close control. The window itself
    * only hides (see [[MarkdownPreviewWindow.resource]]'s disposal-on-release comment) -- the callback's only job is to
    * let the caller sync the toggle back off in application state, mirroring `SwingWindow.setOnResize`'s synchronous
    * callback-registration shape rather than returning `IO[Unit]` for a call that never blocks.
    */
  def setOnUserClose(callback: () => Unit): Unit

object MarkdownPreviewWindow:

  private val WindowTitle: String = "Serenity -- Markdown Preview"
  private val DefaultSize         = new Dimension(640, 800)
  val PreviewFont: java.awt.Font  = new java.awt.Font(java.awt.Font.SANS_SERIF, java.awt.Font.PLAIN, 14)

  private class ImagePanel extends JPanel:
    private val imageRef = new AtomicReference[Option[BufferedImage]](None)

    def setImage(image: BufferedImage): Unit =
      imageRef.set(Some(image))
      repaint()

    override def paintComponent(g: Graphics): Unit =
      super.paintComponent(g)
      imageRef.get().foreach(image => g.drawImage(image, 0, 0, getWidth, getHeight, null))

  private def runOnEdt(action: => Unit): IO[Unit] =
    IO.blocking {
      if SwingUtilities.isEventDispatchThread then action
      else SwingUtilities.invokeAndWait(() => action)
    }

  /** Builds the window once for the TUI process's lifetime -- hidden until the preview is first toggled on -- and
    * disposes it only when this `Resource` is released, i.e. at TUI shutdown (quit or crash unwinding the `Resource`
    * stack `TuiRuntime.run` builds), mirroring `SwingWindow.resource`'s quit/crash-safe disposal. Toggling the preview
    * merely shows/hides the same frame rather than repeatedly constructing and disposing one.
    */
  def resource: Resource[IO, MarkdownPreviewWindow] =
    Resource.make(acquire)(release).map(_._2)

  private def acquire: IO[(JFrame, MarkdownPreviewWindow)] =
    IO.blocking {
      val onCloseRef = new AtomicReference[() => Unit](() => ())
      val panel      = new ImagePanel
      val frame      = new JFrame(WindowTitle)
      frame.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE)
      frame.setFocusableWindowState(false)
      frame.getContentPane.setLayout(new BorderLayout)
      frame.getContentPane.add(panel, BorderLayout.CENTER)
      frame.setSize(DefaultSize)
      frame.addWindowListener(
        new WindowAdapter:
          override def windowClosing(e: WindowEvent): Unit =
            frame.setVisible(false)
            onCloseRef.get().apply()
      )
      val window = new MarkdownPreviewWindow:
        def show(): IO[Unit]                            = runOnEdt(frame.setVisible(true))
        def hide(): IO[Unit]                            = runOnEdt(frame.setVisible(false))
        def updateImage(image: BufferedImage): IO[Unit] = runOnEdt(panel.setImage(image))
        def currentSize: IO[(Int, Int)]                 = IO.blocking((panel.getWidth, panel.getHeight))
        def setOnUserClose(callback: () => Unit): Unit  = onCloseRef.set(callback)
      (frame, window)
    }

  private def release(acquired: (JFrame, MarkdownPreviewWindow)): IO[Unit] =
    runOnEdt(acquired._1.dispose())

end MarkdownPreviewWindow

/** Whether the TUI process can offer the Swing preview window at all -- decided once at startup from
  * `LaunchOptions.isDisplayReachable` (issue #1113: an SSH session with no X11/Wayland forwarding must report
  * unavailability rather than either silently no-op'ing or risking a `HeadlessException`).
  */
sealed trait MarkdownPreviewWindowAvailability

object MarkdownPreviewWindowAvailability:
  case object Unavailable                                   extends MarkdownPreviewWindowAvailability
  final case class Available(window: MarkdownPreviewWindow) extends MarkdownPreviewWindowAvailability
