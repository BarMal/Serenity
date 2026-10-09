package com.serenity.config

import java.nio.file.{Files, Path}

import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Auto-save is opt-in (#1992): nothing in an older or damaged config file may switch it on. */
class AutoSaveConfigSpec extends AnyFlatSpec with Matchers:

  private def load(text: String): ConfigLoadResult =
    val file: Path = Files.createTempFile("serenity-auto-save", ".conf")
    try
      Files.writeString(file, text)
      ConfigManager.loadConfigResultIO(Some(file.toString)).unsafeRunSync() match
        case Right(result) => result
        case Left(error)   => fail(s"expected the load to succeed, received $error")
    finally Files.deleteIfExists(file): Unit

  "auto-save" should "be off with a one second delay by default" in {
    AppConfig.default.autoSaveConfig shouldBe AutoSaveConfig(AutoSaveMode.Off, 1000L)
  }

  it should "read every mode from the config file" in {
    val modes = List(
      "off"              -> AutoSaveMode.Off,
      "after-delay"      -> AutoSaveMode.AfterDelay,
      "on-focus-change"  -> AutoSaveMode.OnFocusChange,
      "on-window-change" -> AutoSaveMode.OnWindowChange
    )
    modes.foreach { (key, mode) =>
      withClue(key)(load(s"files.auto_save = $key").config.autoSaveConfig.mode shouldBe mode)
    }
  }

  it should "read the delay from the config file" in {
    load("files.auto_save_delay_ms = 2500").config.autoSaveConfig.delayMillis shouldBe 2500L
  }

  it should "keep the default and report the entry when the mode is not one it knows" in {
    val result = load("files.auto_save = sometimes")

    result.config.autoSaveConfig.mode shouldBe AutoSaveMode.Off
    result.report.invalidEntries.map(_.key) should contain("files.auto_save")
  }

  it should "keep the default and report the entry when the delay would save while the user is still typing" in {
    val result = load("files.auto_save_delay_ms = 20")

    result.config.autoSaveConfig.delayMillis shouldBe AutoSaveConfig.DefaultDelayMillis
    result.report.invalidEntries.map(_.key) should contain("files.auto_save_delay_ms")
  }

  it should "keep a valid mode when the delay beside it is invalid" in {
    val result = load("files.auto_save = after-delay\nfiles.auto_save_delay_ms = nonsense")

    result.config.autoSaveConfig shouldBe AutoSaveConfig(AutoSaveMode.AfterDelay, AutoSaveConfig.DefaultDelayMillis)
  }

  it should "write the default config with auto-save off" in {
    val text = ConfigManager.configToString(AppConfig.default)

    text should include("files.auto_save = off")
    text should include("files.auto_save_delay_ms = 1000")
  }
