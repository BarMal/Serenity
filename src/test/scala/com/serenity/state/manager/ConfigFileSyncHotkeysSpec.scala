package com.serenity.state.manager

import java.nio.file.{Files, Path}

import cats.effect.unsafe.implicits.global
import com.serenity.TestTemp
import com.serenity.config.{AppConfig, ConfigManager, ConfigManagerTestSupport, HotkeyAction, HotkeyConfig}
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Hot reload re-applies the file on top of the platform defaults, so a file that names only the user's changes reloads
  * as those changes, and an old file that pins a default reloads as the current default.
  */
class ConfigFileSyncHotkeysSpec extends AnyFlatSpec with Matchers with OptionValues:

  private val linuxPlan = ConfigManagerTestSupport.linuxPlan

  private def reloaded(fileText: String): HotkeyConfig =
    val file: Path = TestTemp.file("serenity-hot-reload", ".conf")
    Files.writeString(file, fileText)
    val sync = ConfigFileSync.unsafe(
      file,
      Some(AppConfig.default),
      ConfigManager.saveConfigIO,
      load = path =>
        cats.effect.IO.blocking(Right(ConfigManagerTestSupport.loadConfigResult(Some(path.toString), linuxPlan)))
    )
    sync.externalChange.unsafeRunSync() match
      case Some(ConfigFileChange.Edited(loaded, _)) => loaded.config.inputConfig.hotkeyConfig
      case other                                    => fail(s"expected an edit to be read, found $other")

  "a hot reload of a slim file" should "apply its overrides over the current defaults" in {
    val hotkeys = reloaded("config.version = 2\nhotkey.redo = [\"ctrl+alt+r\"]\n")

    hotkeys.bindingsFor(HotkeyAction.Redo).map(_.render) shouldBe List("ctrl+alt+r")
    hotkeys.bindingsFor(HotkeyAction.Save) shouldBe HotkeyConfig.forOs("linux").bindingsFor(HotkeyAction.Save)
  }

  "a hot reload of a file that pins an old default" should "bring the current default back" in {
    val hotkeys = reloaded("config.version = 1\nhotkey.redo = [\"ctrl+y\"]\n")

    hotkeys.bindingsFor(HotkeyAction.Redo) shouldBe HotkeyConfig.forOs("linux").bindingsFor(HotkeyAction.Redo)
    hotkeys.bindingsFor(HotkeyAction.Redo).map(_.render) should contain("ctrl+shift+z")
  }
