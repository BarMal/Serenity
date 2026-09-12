package com.serenity

import java.nio.file.Files

import cats.effect.unsafe.implicits.global
import com.serenity.config.*
import com.serenity.lsp.config.{LanguageId, LspServerOverride, LspUserConfig}
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** LSP language server override configuration: command/args/enabled overrides and HOCON argument list round-tripping. */
class ConfigManagerLspConfigSpec extends AnyFlatSpec with Matchers with OptionValues:

  "ConfigManager" should "load and write LSP language server overrides" in {
    val configFile = Files.createTempFile("serenity-lsp-config", ".conf")
    Files.writeString(
      configFile,
      """lsp.scala.enabled = false
        |lsp.python.command = pylsp
        |lsp.python.args = --stdio,--log-file,/tmp/pylsp.log
        |""".stripMargin
    )

    val config = ConfigManager.loadConfig(Some(configFile.toString))

    config.languageToolsConfig.lspUserConfig.servers.value(LanguageId.Scala.id) shouldBe LspServerOverride(
      command = None,
      args = None,
      enabled = Some(false)
    )
    config.languageToolsConfig.lspUserConfig.servers.value(LanguageId.Python.id) shouldBe LspServerOverride(
      command = Some("pylsp"),
      args = Some(List("--stdio", "--log-file", "/tmp/pylsp.log")),
      enabled = None
    )

    val written = ConfigManager.configToString(config)
    written should include("lsp.scala.enabled = false")
    written should include("lsp.python.command = pylsp")
    written should include("lsp.python.args = [\"--stdio\", \"--log-file\", \"/tmp/pylsp.log\"]")
  }

  it should "preserve commas inside HOCON LSP argument list values" in {
    val configFile = Files.createTempFile("serenity-lsp-comma-args", ".conf")
    Files.writeString(configFile, "lsp.python.args = [\"--define=A,B\", \"--stdio\"]\n")

    val config = ConfigManager.loadConfig(Some(configFile.toString))

    config.languageToolsConfig.lspUserConfig.servers.value(LanguageId.Python.id).args shouldBe Some(
      List("--define=A,B", "--stdio")
    )
    val written = ConfigManager.configToString(config)
    Files.writeString(configFile, written)
    ConfigManager
      .loadConfig(Some(configFile.toString))
      .languageToolsConfig
      .lspUserConfig
      .servers
      .value(LanguageId.Python.id)
      .args shouldBe Some(
      List("--define=A,B", "--stdio")
    )
  }

  it should "round-trip an empty HOCON LSP argument list through structured loading" in {
    val configFile = Files.createTempFile("serenity-lsp-empty-args", ".conf")
    val config = AppConfig.default.withLspUserConfig(
      LspUserConfig(
        servers = Some(
          Map(
            LanguageId.Python.id -> LspServerOverride(
              command = None,
              args = Some(Nil)
            )
          )
        )
      )
    )
    Files.writeString(configFile, ConfigManager.configToString(config))

    ConfigManager.loadConfigResultIO(Some(configFile.toString)).unsafeRunSync() match
      case Right(result) =>
        result.config.languageToolsConfig.lspUserConfig.servers.value(LanguageId.Python.id).args shouldBe Some(Nil)
      case Left(error) => fail(s"expected structured round-trip success, received $error")
  }
