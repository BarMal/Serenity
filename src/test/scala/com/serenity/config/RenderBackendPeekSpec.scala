package com.serenity.config

import java.nio.file.{Files, Path}

import cats.effect.unsafe.implicits.global
import com.serenity.TestTemp
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RenderBackendPeekSpec extends AnyFlatSpec with Matchers:

  private def peek(content: String): RenderBackendSetting =
    val file = TestTemp.file("serenity-render-backend", ".conf")
    Files.writeString(file, content)
    RenderBackendPeek.read(file).unsafeRunSync()

  "RenderBackendPeek.read" should "read ui.render.backend from the config file" in {
    peek("ui.render.backend = skia\n") shouldBe RenderBackendSetting.Skia
    peek("ui.render.backend = java2d\nui.render.fps = 60\n") shouldBe RenderBackendSetting.Java2D
  }

  it should "default to auto when the key is absent, invalid or the file is unreadable" in {
    peek("ui.render.fps = 60\n") shouldBe RenderBackendSetting.Auto
    peek("ui.render.backend = vulkan\n") shouldBe RenderBackendSetting.Auto
    peek("ui.render.backend = {{{") shouldBe RenderBackendSetting.Auto
    val missing: Path = TestTemp.file("serenity-render-backend-missing", ".conf")
    Files.delete(missing)
    RenderBackendPeek.read(missing).unsafeRunSync() shouldBe RenderBackendSetting.Auto
  }

  it should "agree with the full config loader" in {
    val file = TestTemp.file("serenity-render-backend-agree", ".conf")
    Files.writeString(file, "ui.render.backend = skia\n")
    ConfigManagerTestSupport.loadConfig(Some(file.toString)).surfaceConfig.renderBackend shouldBe
      RenderBackendPeek.read(file).unsafeRunSync()
  }

  "A rendered default config" should "carry the backend setting" in {
    ConfigManager.configToString(AppConfig.default) should include("ui.render.backend = auto")
  }
