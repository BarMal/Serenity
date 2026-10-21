package com.serenity.config

import java.nio.file.Files

import cats.effect.unsafe.implicits.global
import com.serenity.TestTemp
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A file that pins chrome keys the target layout would set differently: which keys those are, and how one is handed
  * back to the layout.
  */
class LayoutOverrideSpec extends AnyFlatSpec with Matchers:

  private val pinnedToConventional = AppConfig.default
    .withLineNumbers(true)
    .withStatusLinePlacement(
      StatusLinePlacement.Floating
    )

  "The pins of a layout" should "name each explicit key whose value the layout would change" in {
    val config = AppConfig.default.withLineNumbers(true).withScrollbar(true)

    LayoutPreset.pins(config, Set("editor.line_numbers", "editor.scrollbar"), UiLayout.Minimal) shouldBe List(
      LayoutPin("editor.line_numbers", current = "true", layoutValue = "false"),
      LayoutPin("editor.scrollbar", current = "true", layoutValue = "false")
    )
  }

  it should "leave out an explicit key that already says what the layout does" in {
    val config = AppConfig.default.withLineNumbers(false)

    LayoutPreset.pins(config, Set("editor.line_numbers"), UiLayout.Minimal) shouldBe Nil
  }

  it should "leave out chrome the file never sets, whatever its value" in {
    LayoutPreset.pins(pinnedToConventional, Set.empty, UiLayout.Minimal) shouldBe Nil
  }

  it should "count a legacy spelling as the file setting the key" in {
    val pins = LayoutPreset.pins(pinnedToConventional, Set("cursor.info_bar.placement"), UiLayout.Minimal)

    pins.map(_.key) shouldBe List("status.placement")
    pins.map(pin => pin.current -> pin.layoutValue) shouldBe List("floating" -> "off")
  }

  it should "read the value the target layout gives, not the one being left" in {
    val config = AppConfig.conventional.withLineNumbers(false)

    LayoutPreset.pins(config, Set("editor.line_numbers"), UiLayout.Conventional) shouldBe List(
      LayoutPin("editor.line_numbers", current = "false", layoutValue = "true")
    )
  }

  it should "give each pin a label a person can read" in {
    LayoutPin("editor.line_numbers", "true", "false").label shouldBe "Editor Line Numbers"
  }

  "Adopting the layout for some keys" should "set those and only those to the layout's value" in {
    val adopted = LayoutPreset.adopt(pinnedToConventional, UiLayout.Minimal, Set("editor.line_numbers"))

    adopted.surfaceConfig.showLineNumbers shouldBe false
    adopted.statusLine.placement shouldBe StatusLinePlacement.Floating
  }

  private def release(text: String, keys: Set[String]): (Either[ConfigError, Set[String]], String) =
    val file = TestTemp.file("serenity-layout-release", ".conf")
    Files.writeString(file, text)
    val result = ConfigManager.releaseKeysIO(file, keys).unsafeRunSync()
    (result, Files.readString(file))

  "Releasing keys from the file" should "remove their lines and leave everything else as written" in {
    val (result, text) = release(
      "# mine\neditor.tab_size = 3\neditor.line_numbers = true\neditor.pane_headers = true\n",
      Set("editor.line_numbers")
    )

    result shouldBe Right(Set.empty)
    text shouldBe "# mine\neditor.tab_size = 3\neditor.pane_headers = true\n"
  }

  it should "remove a key under every spelling the file may use" in {
    val (result, text) = release("cursor.info_bar.placement = floating\neditor.tab_size = 3\n", Set("status.placement"))

    result shouldBe Right(Set.empty)
    text shouldBe "editor.tab_size = 3\n"
  }

  it should "report a key it could not reach, such as one written inside a block, and leave the file alone" in {
    val original       = "editor {\n  line_numbers = true\n}\neditor.tab_size = 3\n"
    val (result, text) = release(original, Set("editor.line_numbers"))

    result shouldBe Right(Set("editor.line_numbers"))
    text shouldBe original
  }

  it should "release what it can and report the rest" in {
    val (result, text) = release(
      "editor {\n  line_numbers = true\n}\neditor.pane_headers = true\n",
      Set("editor.line_numbers", "editor.pane_headers")
    )

    result shouldBe Right(Set("editor.line_numbers"))
    text should not include "pane_headers"
    text should include("line_numbers = true")
  }

  it should "keep a backup of the file it edited" in {
    val file = TestTemp.file("serenity-layout-release-backup", ".conf")
    Files.writeString(file, "editor.line_numbers = true\n")

    ConfigManager.releaseKeysIO(file, Set("editor.line_numbers")).unsafeRunSync() shouldBe Right(Set.empty)

    val siblings = Files.list(file.getParent).toArray.toList.map(_.toString)
    siblings.exists(_.contains(s"${file.getFileName}.bak-")) shouldBe true
  }

  it should "touch nothing when no key is asked for" in {
    val original       = "editor.line_numbers = true\n"
    val (result, text) = release(original, Set.empty)

    result shouldBe Right(Set.empty)
    text shouldBe original
  }

  it should "make the layout's value apply again when the file is next read" in {
    val file = TestTemp.file("serenity-layout-release-read", ".conf")
    Files.writeString(file, "editor.line_numbers = true\n")

    ConfigManager.releaseKeysIO(file, Set("editor.line_numbers")).unsafeRunSync()

    ConfigManagerTestSupport.loadConfig(Some(file.toString)).surfaceConfig.showLineNumbers shouldBe false
  }
