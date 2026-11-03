package com.serenity.config

import java.nio.file.Files

import com.serenity.TestTemp
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** `explorer.open_mode` chooses how a click in the explorer opens a file (#2479). */
class ExplorerOpenModeConfigSpec extends AnyFlatSpec with Matchers:

  private def loaded(content: String): ConfigLoadResult =
    val file = TestTemp.file("serenity-explorer-open-mode", ".conf")
    Files.writeString(file, content)
    ConfigManagerTestSupport.loadConfigResult(Some(file.toString))

  "The explorer open mode" should "default to single-click" in {
    AppConfig.default.inputConfig.explorerOpenMode shouldBe ExplorerOpenMode.SingleClick
    ConfigRegistry.defaultFor("explorer.open_mode").map(_.rendered) shouldBe Some("single-click")
  }

  it should "be written to the reference config" in {
    ConfigRegistry.writtenKeys should contain("explorer.open_mode")
  }

  it should "read each of its three values" in {
    loaded("explorer.open_mode = double-click\n").config.inputConfig.explorerOpenMode shouldBe
      ExplorerOpenMode.DoubleClick
    loaded("explorer.open_mode = single-click\n").config.inputConfig.explorerOpenMode shouldBe
      ExplorerOpenMode.SingleClick
    loaded("explorer.open_mode = preview\n").config.inputConfig.explorerOpenMode shouldBe ExplorerOpenMode.Preview
  }

  it should "ignore case and surrounding space" in {
    loaded("explorer.open_mode =  Preview \n").config.inputConfig.explorerOpenMode shouldBe ExplorerOpenMode.Preview
  }

  it should "report an unknown value with the accepted choices and keep the default" in {
    val result = loaded("explorer.open_mode = triple-click\n")
    val entry  = result.report.invalidEntries.find(_.key == "explorer.open_mode")
    entry.map(_.value) shouldBe Some("triple-click")
    entry.map(_.reason).getOrElse("") should include("double-click, single-click or preview")
    result.config.inputConfig.explorerOpenMode shouldBe ExplorerOpenMode.SingleClick
  }

  it should "be reported as nothing when valid" in {
    loaded("explorer.open_mode = preview\n").report.invalidEntries shouldBe Nil
  }

  "ExplorerOpenMode.fromConfigKey" should "name each mode by its key" in {
    ExplorerOpenMode.values.toList.map(mode => ExplorerOpenMode.fromConfigKey(mode.configKey)) shouldBe
      ExplorerOpenMode.values.toList.map(Some(_))
  }
