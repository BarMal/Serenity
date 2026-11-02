package com.serenity.perf

import com.serenity.state.manager.RenderCaches
import com.serenity.state.models.AppState
import com.serenity.ui.renderer.{FontSpec, RendererEntryPoints}
import com.serenity.{FrameBackend, FramePixels}

/** Frames of the laptop-sized window drawn from scratch with a chosen renderer, as the reference the frame-reuse specs
  * compare their persisting frames with.
  */
private[perf] object LaptopColdFrames:

  import LaptopFrameBenchmarks.{DeviceScale, LogicalHeightPx, LogicalWidthPx, cellMetrics, codeFont, textFont, viewport}

  val DeviceWidthPx: Int  = math.ceil(LogicalWidthPx * DeviceScale).toInt
  val DeviceHeightPx: Int = math.ceil(LogicalHeightPx * DeviceScale).toInt

  def of(backend: FrameBackend, state: AppState, caches: RenderCaches, cursorVisible: Boolean = true): FramePixels =
    val frame = backend.newFrame(DeviceWidthPx, DeviceHeightPx)
    val surface = backend.surfaceOver(
      frame,
      cellMetrics,
      codeFont,
      LogicalWidthPx,
      LogicalHeightPx,
      DeviceScale,
      persists = false,
      caches
    )
    RendererEntryPoints.render(
      state,
      cursorVisible,
      surface,
      viewport,
      FontSpec.fromAwt(codeFont),
      FontSpec.fromAwt(textFont),
      cellMetrics,
      None,
      caches
    )
    surface.flush()
    frame
