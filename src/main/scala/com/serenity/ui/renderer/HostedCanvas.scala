package com.serenity.ui.renderer

import java.awt.Component

import com.serenity.ui.layout.CellMetrics

/** What a hosted canvas needs to know to build the surface for one frame. `screen` is the same value every frame, so
  * caches keyed by it survive from one frame to the next.
  */
final case class HostedFrame(metrics: CellMetrics, font: FontSpec, screen: ScreenIdentity)

/** How a frame drawn inside a hosted canvas ended. */
enum HostedOutcome:
  case Drawn
  case Skipped(reason: String)
  case Failed(cause: Throwable)

/** A window canvas that draws each frame inside its own render callback, as a GPU layer must: the callback owns the
  * `Canvas`, and a window's GPU buffer does not keep the previous frame. It takes the place of the Java2D `JPanel`'s
  * painting, so no finished image is handed to the window and the carets are drawn into the frame itself.
  *
  * The frame scheduler stays in charge. [[drawFrame]] is called from the frame loop and returns once the frame has been
  * drawn. The canvas draws nothing else, except to repeat the last frame when the toolkit exposes or resizes it.
  */
trait HostedCanvas:
  /** The component that gets mouse, keyboard and focus events; the window adds it where its Java2D canvas painted. */
  def component: Component

  /** The graphics API the canvas draws with, for logs and benchmark reports. */
  def renderApi: String

  def drawFrame(frame: HostedFrame)(paint: RenderSurface => Unit): HostedOutcome
