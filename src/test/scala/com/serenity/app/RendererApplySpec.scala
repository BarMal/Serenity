package com.serenity.app

import java.nio.file.{Files, Path}

import cats.effect.unsafe.implicits.global
import com.serenity.TestTemp
import com.serenity.app.RendererChoice.Candidate
import com.serenity.config.{AppConfig, ConfigManager, RenderBackendSetting, SkiaFrames}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RendererApplySpec extends AnyFlatSpec with Matchers:

  private def path(): Path = TestTemp.directory("renderer-apply").resolve("config.conf")

  private def loaded(file: Path): AppConfig = ConfigManager.loadConfigIO(Some(file.toString)).unsafeRunSync()

  private def withSurface(file: Path, backend: RenderBackendSetting, frames: SkiaFrames): Unit =
    val config = AppConfig.default
    val saved = ConfigManager
      .saveConfigIO(config.withSurfaceConfig(config.surfaceConfig.copy(renderBackend = backend, renderSkiaFrames = frames)), file)
      .unsafeRunSync()
    saved shouldBe Right(())

  "RendererApply" should "write the GPU layer with image frames to the config" in {
    val file = path()
    withSurface(file, RenderBackendSetting.SkiaRaster, SkiaFrames.Picture)
    RendererApply(Candidate.LayerImage, file).unsafeRunSync() shouldBe Right(())
    val surface = loaded(file).surfaceConfig
    (surface.renderBackend, surface.renderSkiaFrames) shouldBe (RenderBackendSetting.Skia, SkiaFrames.Image)
  }

  it should "write the GPU layer with picture frames to the config" in {
    val file = path()
    RendererApply(Candidate.LayerPicture, file).unsafeRunSync() shouldBe Right(())
    val surface = loaded(file).surfaceConfig
    (surface.renderBackend, surface.renderSkiaFrames) shouldBe (RenderBackendSetting.Skia, SkiaFrames.Picture)
  }

  it should "write the Skia raster surface and leave the layer's frame form as it was" in {
    val file = path()
    withSurface(file, RenderBackendSetting.Skia, SkiaFrames.Picture)
    RendererApply(Candidate.SkiaRaster, file).unsafeRunSync() shouldBe Right(())
    val surface = loaded(file).surfaceConfig
    (surface.renderBackend, surface.renderSkiaFrames) shouldBe (RenderBackendSetting.SkiaRaster, SkiaFrames.Picture)
  }

  it should "keep every other setting of an existing config" in {
    val file   = path()
    val before = AppConfig.default
    val edited = before.withSurfaceConfig(before.surfaceConfig.copy(renderSkiaVsync = true))
    ConfigManager.saveConfigIO(edited, file).unsafeRunSync() shouldBe Right(())
    RendererApply(Candidate.LayerImage, file).unsafeRunSync() shouldBe Right(())
    loaded(file).surfaceConfig.renderSkiaVsync shouldBe true
  }

  it should "create the config when there is none" in {
    val file = path()
    Files.exists(file) shouldBe false
    RendererApply(Candidate.LayerImage, file).unsafeRunSync() shouldBe Right(())
    Files.exists(file) shouldBe true
  }

  it should "say why, rather than throw, when the config cannot be written" in {
    val directory = TestTemp.directory("renderer-apply-blocked")
    val blocker   = directory.resolve("blocker")
    Files.writeString(blocker, "a file, not a directory")
    RendererApply(Candidate.LayerImage, blocker.resolve("config.conf")).unsafeRunSync() match
      case Left(message) => message should not be empty
      case Right(_)      => fail("a config under a file cannot be written")
  }
