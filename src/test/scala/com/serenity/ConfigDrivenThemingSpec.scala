package com.serenity

import java.nio.file.{Files, Path, Paths}

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.ui.theme.config.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import pureconfig.*
import pureconfig.generic.derivation.default.*

class ConfigDrivenThemingSpec extends AnyFlatSpec with Matchers:

  "ThemeConfig" should "be loadable from HOCON configuration" in {
    val configSource = ConfigSource.string("""
      theme {
        name = "test-theme"
        colors {
          foreground = "white"
          background = "black" 
          cursor = "yellow"
        }
        syntax {
          keyword {
            foreground = "blue"
            style {
              bold = true
              italic = false
              underline = false
            }
          }
          string {
            foreground = "green"
            style {
              bold = false
              italic = false  
              underline = false
            }
          }
          comment {
            foreground = "gray"
            style {
              bold = false
              italic = true
              underline = false
            }
          }
          number {
            foreground = "cyan"
            style {
              bold = false
              italic = false
              underline = false
            }
          }
          operator {
            foreground = "yellow"
            style {
              bold = false
              italic = false
              underline = false
            }
          }
          identifier {
            foreground = "white"
            style {
              bold = false
              italic = false
              underline = false
            }
          }
          typ {
            foreground = "magenta"
            style {
              bold = true
              italic = false
              underline = false
            }
          }
          delimiter {
            foreground = "white"
            style {
              bold = false
              italic = false
              underline = false
            }
          }
          whitespace {
            foreground = "black"
            style {
              bold = false
              italic = false
              underline = false
            }
          }
          error {
            foreground = "red"
            style {
              bold = false
              italic = false
              underline = true
            }
          }
          normal {
            foreground = "white"
            style {
              bold = false
              italic = false
              underline = false
            }
          }
        }
      }
    """)

    val themeConfig = configSource.at("theme").load[ThemeConfig]

    themeConfig shouldBe a[Right[?, ?]]
    themeConfig.toOption.get.name shouldBe "test-theme"
    themeConfig.toOption.get.colors.foreground shouldBe "white"
    themeConfig.toOption.get.syntax.keyword.foreground shouldBe "blue"
    themeConfig.toOption.get.syntax.keyword.style.bold shouldBe true
    themeConfig.toOption.get.syntax.comment.style.italic shouldBe true
  }

  "ThemeConfigLoader" should "load theme from file" in {
    val loader = new ThemeConfigLoader()

    // This test will verify that we can load from a file
    // We'll create a temporary theme file for testing
    val tempFile = Files.createTempFile("test-theme", ".conf")
    Files.writeString(
      tempFile,
      """
      theme {
        name = "file-theme"
        colors {
          foreground = "white"
          background = "black"
          cursor = "yellow"
        }
        syntax {
          keyword {
            foreground = "purple"
            style { bold = true, italic = false, underline = false }
          }
          string {
            foreground = "green"
            style { bold = false, italic = false, underline = false }
          }
          comment {
            foreground = "gray"
            style { bold = false, italic = true, underline = false }
          }
          number {
            foreground = "cyan"
            style { bold = false, italic = false, underline = false }
          }
          operator {
            foreground = "yellow"
            style { bold = false, italic = false, underline = false }
          }
          identifier {
            foreground = "white"
            style { bold = false, italic = false, underline = false }
          }
        }
      }
    """
    )

    try
      val result = loader.loadThemeFromFile(tempFile).unsafeRunSync()
      result.name shouldBe "file-theme"
      result.syntax.keyword.foreground shouldBe "purple"
    finally Files.deleteIfExists(tempFile)
  }

  "ConfigurableThemeManager" should "convert config to Theme object" in {
    val themeConfig = ThemeConfig(
      name = "test",
      colors = BaseColors(
        foreground = "white",
        background = "black",
        cursor = "yellow"
      ),
      syntax = SyntaxColors(
        keyword = SyntaxElementConfig("blue", Some("black"), StyleConfig(bold = true, italic = false, underline = false)),
        string = SyntaxElementConfig("green", Some("black"), StyleConfig(bold = false, italic = false, underline = false)),
        comment = SyntaxElementConfig("gray", Some("black"), StyleConfig(bold = false, italic = true, underline = false)),
        number = SyntaxElementConfig("cyan", Some("black"), StyleConfig(bold = false, italic = false, underline = false)),
        operator = SyntaxElementConfig("yellow", Some("black"), StyleConfig(bold = false, italic = false, underline = false)),
        identifier = SyntaxElementConfig("white", Some("black"), StyleConfig(bold = false, italic = false, underline = false))
      )
    )

    val themeEither = ConfigurableThemeManager.configToTheme(themeConfig)

    themeEither shouldBe a[Right[?, ?]]
    val theme = themeEither.toOption.get

    theme.name shouldBe "test"
    theme.foregroundColor.toString should include("WHITE") // Lanterna color representation
    theme.syntaxColors should contain key com.serenity.ui.theme.SyntaxElement.Keyword

    val keywordColor = theme.colorFor(com.serenity.ui.theme.SyntaxElement.Keyword)
    keywordColor.style.isBold shouldBe true
    keywordColor.style.isItalic shouldBe false
  }

  "ThemeReloader" should "reload theme configuration dynamically" in {
    // Create a temporary theme file
    val tempFile = Files.createTempFile("reload-theme", ".conf")

    def writeThemeConfig(foregroundColor: String) =
      Files.writeString(
        tempFile,
        s"""
        theme {
          name = "reload-test"
          colors {
            foreground = "$foregroundColor"
            background = "black"
            cursor = "yellow"
          }
          syntax {
            keyword {
              foreground = "blue"
              style { bold = true, italic = false, underline = false }
            }
            string {
              foreground = "green" 
              style { bold = false, italic = false, underline = false }
            }
            comment {
              foreground = "gray"
              style { bold = false, italic = true, underline = false }
            }
            number {
              foreground = "cyan"
              style { bold = false, italic = false, underline = false }
            }
            operator {
              foreground = "yellow"
              style { bold = false, italic = false, underline = false }
            }
            identifier {
              foreground = "white"
              style { bold = false, italic = false, underline = false }
            }
            typ {
              foreground = "magenta"
              style { bold = true, italic = false, underline = false }
            }
            delimiter {
              foreground = "white"
              style { bold = false, italic = false, underline = false }
            }
            whitespace {
              foreground = "black"
              style { bold = false, italic = false, underline = false }
            }
            error {
              foreground = "red"
              style { bold = false, italic = false, underline = true }
            }
            normal {
              foreground = "white"
              style { bold = false, italic = false, underline = false }
            }
          }
        }
      """
      )

    try
      // Write initial config
      writeThemeConfig("white")

      val reloader     = new ThemeReloader()
      val initialTheme = reloader.loadAndConvertTheme(tempFile).unsafeRunSync()
      initialTheme.colors.foreground shouldBe "white"

      // Update config file
      writeThemeConfig("yellow")

      // Reload
      val reloadedTheme = reloader.loadAndConvertTheme(tempFile).unsafeRunSync()
      reloadedTheme.colors.foreground shouldBe "yellow"

    finally Files.deleteIfExists(tempFile)
  }
