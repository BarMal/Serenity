package com.serenity.app

import java.nio.file.Path

import cats.effect.IO
import com.serenity.app.RendererChoice.Candidate
import com.serenity.config.{ConfigManager, RenderBackendSetting, SkiaFrames}

/** `--optimize-renderer --apply`: writes the recommended mode into `config.conf` and leaves every other setting as it
  * was (#2618). The raster surface has no frame form, so it leaves `ui.render.skia_frames` alone.
  */
object RendererApply:

  def apply(candidate: Candidate, configPath: Path): IO[Either[String, Unit]] =
    ConfigManager.loadConfigIO(Some(configPath.toString)).flatMap { config =>
      val surface = config.surfaceConfig
      val chosen = candidate match
        case Candidate.LayerImage =>
          surface.copy(renderBackend = RenderBackendSetting.Skia, renderSkiaFrames = SkiaFrames.Image)
        case Candidate.LayerPicture =>
          surface.copy(renderBackend = RenderBackendSetting.Skia, renderSkiaFrames = SkiaFrames.Picture)
        case Candidate.SkiaRaster => surface.copy(renderBackend = RenderBackendSetting.SkiaRaster)
      ConfigManager.saveConfigIO(config.withSurfaceConfig(chosen), configPath).map(_.left.map(_.message))
    }
