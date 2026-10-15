package com.serenity.config

import java.nio.file.Files

import cats.effect.unsafe.implicits.global
import com.serenity.TestTemp
import com.serenity.config.AppConfigOps.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class UiLayoutSpec extends AnyFlatSpec with Matchers:

  private def loaded(text: String): ConfigLoadResult =
    val file = TestTemp.file("serenity-ui-layout-config", ".conf")
    Files.writeString(file, text)
    ConfigManagerTestSupport.loadConfigResult(Some(file.toString))

  private def chrome(config: AppConfig): (Boolean, Boolean, StatusLinePlacement, TabStripMode, Boolean) =
    (
      config.surfaceConfig.showLineNumbers,
      config.surfaceConfig.showPaneHeaders,
      config.statusLine.placement,
      config.surfaceConfig.tabStrip,
      config.surfaceConfig.scrollbarEnabled
    )

  /** The settings a file assigns; its comments name the same keys without setting them. */
  private def assignments(text: String): List[String] =
    text.linesIterator.map(_.trim).filterNot(line => line.isEmpty || line.startsWith("#")).toList

  private def assigns(text: String, key: String): Boolean = assignments(text).exists(_.startsWith(s"$key ="))

  private val version = s"config.version = ${ConfigVersion.Current.value}\n"

  private val chromeKeys =
    List("editor.line_numbers", "editor.pane_headers", "status.placement", "editor.tab_strip", "editor.scrollbar")

  private val minimalChrome      = (false, false, StatusLinePlacement.Off, TabStripMode.Off, false)
  private val conventionalChrome = (true, true, StatusLinePlacement.Pinned, TabStripMode.Auto, true)

  "The minimal layout" should "be what a fresh install shows" in {
    AppConfig.default.uiLayout shouldBe UiLayout.Minimal
    chrome(AppConfig.default) shouldBe minimalChrome
    AppConfig.default.menuBarMode shouldBe MenuBarMode.Hidden
    ConfigRegistry.defaultFor("ui.layout").map(_.rendered) shouldBe Some("minimal")
    ConfigRegistry.defaultFor("editor.tab_strip").map(_.rendered) shouldBe Some("off")
  }

  "The conventional layout" should "keep every piece of chrome that earlier builds showed" in {
    AppConfig.conventional.uiLayout shouldBe UiLayout.Conventional
    chrome(AppConfig.conventional) shouldBe conventionalChrome
    AppConfig.conventional.menuBarMode shouldBe MenuBarMode.Hidden
  }

  it should "differ from the minimal layout in the layout key and the five chrome settings only" in {
    val asMinimal = AppConfig.conventional
      .withUiLayout(UiLayout.Minimal)
      .withLineNumbers(false)
      .withPaneHeaders(false)
      .withStatusLinePlacement(StatusLinePlacement.Off)
      .withTabStrip(TabStripMode.Off)
      .withScrollbar(false)

    asMinimal shouldBe AppConfig.default
  }

  "A config file" should "start from the minimal layout when it does not say" in {
    chrome(loaded("editor.tab_size = 3\n").config) shouldBe minimalChrome
  }

  it should "start from the conventional layout when it asks for it" in {
    val result = loaded("ui.layout = conventional\n")

    result.config.uiLayout shouldBe UiLayout.Conventional
    chrome(result.config) shouldBe conventionalChrome
    result.report.invalidEntries shouldBe Nil
  }

  it should "let every key it writes win over the layout, whichever comes first" in {
    val before = loaded("editor.line_numbers = false\nstatus.placement = floating\nui.layout = conventional\n").config
    val after  = loaded("ui.layout = conventional\neditor.line_numbers = false\nstatus.placement = floating\n").config

    before shouldBe after
    chrome(before) shouldBe (false, true, StatusLinePlacement.Floating, TabStripMode.Auto, true)
  }

  it should "let a key switch one piece of chrome back on over the minimal layout" in {
    val config = loaded("ui.layout = minimal\neditor.scrollbar = true\neditor.tab_strip = auto\n").config

    chrome(config) shouldBe (false, false, StatusLinePlacement.Off, TabStripMode.Auto, true)
  }

  it should "report an unknown layout and fall back to the minimal one" in {
    val result = loaded("ui.layout = roomy\n")

    result.report.invalidEntries.map(_.key) should contain("ui.layout")
    chrome(result.config) shouldBe minimalChrome
  }

  it should "report an unknown tab strip mode and keep the layout's own" in {
    val result = loaded("ui.layout = conventional\neditor.tab_strip = sometimes\n")

    result.report.invalidEntries.map(_.key) should contain("editor.tab_strip")
    result.config.surfaceConfig.tabStrip shouldBe TabStripMode.Auto
  }

  it should "be a known key and write the layout back" in {
    ConfigKeySchema.isKnownKey("ui.layout") shouldBe true
    ConfigKeySchema.isKnownKey("editor.tab_strip") shouldBe true
    ConfigManager.configToString(AppConfig.conventional) should include("ui.layout = conventional")
    ConfigManager.configToString(AppConfig.default) should include("ui.layout = minimal")
  }

  "Changing the layout" should "apply without a restart" in {
    ConfigRestart.changed(AppConfig.default, AppConfig.conventional) shouldBe Nil
  }

  "Re-basing a config onto another layout" should "move every piece of chrome the file does not set" in {
    chrome(LayoutPreset.rebase(AppConfig.default, UiLayout.Conventional, Set.empty)) shouldBe conventionalChrome
    chrome(LayoutPreset.rebase(AppConfig.conventional, UiLayout.Minimal, Set.empty)) shouldBe minimalChrome
  }

  it should "leave alone a setting the file sets, even when it matches the layout being left" in {
    val rebased = LayoutPreset.rebase(AppConfig.default, UiLayout.Conventional, Set("editor.line_numbers"))

    rebased.uiLayout shouldBe UiLayout.Conventional
    chrome(rebased) shouldBe (false, true, StatusLinePlacement.Pinned, TabStripMode.Auto, true)
  }

  it should "leave alone a setting changed in this session" in {
    val customised = AppConfig.default.withStatusLinePlacement(StatusLinePlacement.Floating)

    LayoutPreset.rebase(customised, UiLayout.Conventional, Set.empty).statusLine.placement shouldBe
      StatusLinePlacement.Floating
  }

  it should "not touch anything but the layout and its five settings" in {
    val rebased = LayoutPreset.rebase(AppConfig.default.withWheelScrollLines(9), UiLayout.Conventional, Set.empty)

    rebased shouldBe AppConfig.conventional.withWheelScrollLines(9)
  }

  it should "count the legacy status line keys as the file setting the status line" in {
    LayoutPreset.chromeKeys("status.placement") should contain("cursor.info_bar.placement")
  }

  "Saving after the layout changed" should "write the layout key and none of the chrome the layout supplies" in {
    val file = TestTemp.file("serenity-ui-layout-save", ".conf")
    Files.writeString(file, s"${version}editor.tab_size = 3\n")
    val switched = LayoutPreset.rebase(
      ConfigManagerTestSupport.loadConfig(Some(file.toString)),
      UiLayout.Conventional,
      Set.empty
    )

    ConfigManager.saveConfigIO(switched, file).unsafeRunSync() shouldBe Right(())

    val text = Files.readString(file)
    text should include("ui.layout = conventional")
    text should include("editor.tab_size = 3")
    chromeKeys.foreach(key => assigns(text, key) shouldBe false)
    ConfigManagerTestSupport.loadConfig(Some(file.toString)) shouldBe switched
  }

  it should "keep the chrome the file sets, and not rewrite it" in {
    val file = TestTemp.file("serenity-ui-layout-save-explicit", ".conf")
    Files.writeString(file, s"$version# mine\neditor.line_numbers = false\nui.layout = minimal\n")
    val loadedConfig = ConfigManagerTestSupport.loadConfig(Some(file.toString))
    val switched     = LayoutPreset.rebase(loadedConfig, UiLayout.Conventional, Set("editor.line_numbers"))

    ConfigManager.saveConfigIO(switched, file).unsafeRunSync() shouldBe Right(())

    val text = Files.readString(file)
    text should include("# mine")
    text should include("editor.line_numbers = false")
    text should include("ui.layout = conventional")
    assigns(text, "status.placement") shouldBe false
    ConfigManagerTestSupport.loadConfig(Some(file.toString)) shouldBe switched
  }

  it should "write a setting changed away from the layout's own value" in {
    val file = TestTemp.file("serenity-ui-layout-save-changed", ".conf")
    Files.writeString(file, s"${version}ui.layout = conventional\n")
    val loadedConfig = ConfigManagerTestSupport.loadConfig(Some(file.toString))

    ConfigManager.saveConfigIO(loadedConfig.withLineNumbers(false), file).unsafeRunSync() shouldBe Right(())

    Files.readString(file) should include("editor.line_numbers = false")
  }

  "Creating a config file" should "write the layout key and leave the layout's own chrome out" in {
    val file = TestTemp.directory("serenity-ui-layout-new").resolve("config.conf")

    ConfigManager.saveConfigIO(AppConfig.default.withWheelScrollLines(9), file).unsafeRunSync() shouldBe Right(())

    val text = Files.readString(file)
    text should include("ui.layout = minimal")
    chromeKeys.foreach(key => assigns(text, key) shouldBe false)
    ConfigManagerTestSupport.loadConfig(Some(file.toString)) shouldBe AppConfig.default.withWheelScrollLines(9)
  }

  it should "write chrome that differs from the layout's own" in {
    val file   = TestTemp.directory("serenity-ui-layout-new-custom").resolve("config.conf")
    val config = AppConfig.default.withLineNumbers(true)

    ConfigManager.saveConfigIO(config, file).unsafeRunSync() shouldBe Right(())

    Files.readString(file) should include("editor.line_numbers = true")
    ConfigManagerTestSupport.loadConfig(Some(file.toString)) shouldBe config
  }
