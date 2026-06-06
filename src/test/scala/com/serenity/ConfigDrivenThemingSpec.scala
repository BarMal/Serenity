package com.serenity

import java.nio.file.Files

import cats.effect.unsafe.implicits.global
import com.serenity.ui.theme.config.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import pureconfig.*

class ConfigDrivenThemingSpec extends AnyFlatSpec with Matchers:

  "ThemeConfig" should "be loadable from HOCON configuration" in {
    val configSource = ConfigSource.string("""
      theme {
        name = "test-theme"
        ui {
          foreground = "#F5F7FA"
          background = "#0B0F14"
          cursor = "#F4D03F"
          border = "#2F3B4A"
          muted = "#7B8794"
          placeholder = "#52606D"
          highlighted { foreground = "#0B0F14", background = "#5DADE2", style { bold = false, italic = false, underline = false } }
          menu-item { foreground = "#F5F7FA", background = "#1F2933", style { bold = false, italic = false, underline = false } }
          panel { foreground = "#F5F7FA", background = "#111821", style { bold = false, italic = false, underline = false } }
          error { foreground = "#FF6B6B", background = "#2B1215", style { bold = false, italic = false, underline = false } }
        }
        syntax {
          keyword {
            foreground = "#5DADE2"
            style {
              bold = true
              italic = false
              underline = false
            }
          }
          string {
            foreground = "#58D68D"
            style {
              bold = false
              italic = false  
              underline = false
            }
          }
          comment {
            foreground = "#808080"
            style {
              bold = false
              italic = true
              underline = false
            }
          }
          number {
            foreground = "#F39C12"
            style {
              bold = false
              italic = false
              underline = false
            }
          }
          operator {
            foreground = "#E74C3C"
            style {
              bold = false
              italic = false
              underline = false
            }
          }
          identifier {
            foreground = "#FFFFFF"
            style {
              bold = false
              italic = false
              underline = false
            }
          }
          typ {
            foreground = "#AF7AC5"
            style {
              bold = true
              italic = false
              underline = false
            }
          }
          delimiter {
            foreground = "#D5D8DC"
            style {
              bold = false
              italic = false
              underline = false
            }
          }
          whitespace {
            foreground = "#0B0F14"
            style {
              bold = false
              italic = false
              underline = false
            }
          }
          error {
            foreground = "#FF6B6B"
            style {
              bold = false
              italic = false
              underline = true
            }
          }
          normal {
            foreground = "#FFFFFF"
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
    themeConfig.toOption.get.ui.foreground shouldBe "#F5F7FA"
    themeConfig.toOption.get.syntax.keyword.foreground shouldBe "#5DADE2"
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
        ui {
          foreground = "#F5F7FA"
          background = "#0B0F14"
          cursor = "#F4D03F"
          border = "#2F3B4A"
          muted = "#7B8794"
          placeholder = "#52606D"
          highlighted { foreground = "#0B0F14", background = "#5DADE2", style { bold = false, italic = false, underline = false } }
          menu-item { foreground = "#F5F7FA", background = "#1F2933", style { bold = false, italic = false, underline = false } }
          panel { foreground = "#F5F7FA", background = "#111821", style { bold = false, italic = false, underline = false } }
          error { foreground = "#FF6B6B", background = "#2B1215", style { bold = false, italic = false, underline = false } }
        }
        syntax {
          keyword {
            foreground = "#AF7AC5"
            style { bold = true, italic = false, underline = false }
          }
          string {
            foreground = "#58D68D"
            style { bold = false, italic = false, underline = false }
          }
          comment {
            foreground = "#808080"
            style { bold = false, italic = true, underline = false }
          }
          number {
            foreground = "#F39C12"
            style { bold = false, italic = false, underline = false }
          }
          operator {
            foreground = "#E74C3C"
            style { bold = false, italic = false, underline = false }
          }
          identifier {
            foreground = "#FFFFFF"
            style { bold = false, italic = false, underline = false }
          }
        }
      }
    """
    )

    try
      val result = loader.loadThemeFromFile(tempFile).unsafeRunSync()
      result.name shouldBe "file-theme"
      result.syntax.keyword.foreground shouldBe "#AF7AC5"
    finally Files.deleteIfExists(tempFile)
  }

  "ConfigurableThemeManager" should "convert config to Theme object" in {
    val themeConfig = ThemeConfig(
      name = "test",
      ui = UiColors(
        foreground = "#F5F7FA",
        background = "#0B0F14",
        cursor = "#F4D03F",
        highlighted = UiTokenConfig(foreground = "#0B0F14", background = "#5DADE2"),
        menuItem = UiTokenConfig(foreground = "#F5F7FA", background = "#1F2933"),
        panel = UiTokenConfig(foreground = "#F5F7FA", background = "#111821"),
        error = UiTokenConfig(foreground = "#FF6B6B", background = "#2B1215"),
        border = "#2F3B4A",
        muted = "#7B8794",
        placeholder = "#52606D"
      ),
      syntax = SyntaxColors(
        keyword =
          SyntaxElementConfig("#5DADE2", Some("#0B0F14"), StyleConfig(bold = true, italic = false, underline = false)),
        string =
          SyntaxElementConfig("#58D68D", Some("#0B0F14"), StyleConfig(bold = false, italic = false, underline = false)),
        comment =
          SyntaxElementConfig("#808080", Some("#0B0F14"), StyleConfig(bold = false, italic = true, underline = false)),
        number =
          SyntaxElementConfig("#F39C12", Some("#0B0F14"), StyleConfig(bold = false, italic = false, underline = false)),
        operator =
          SyntaxElementConfig("#E74C3C", Some("#0B0F14"), StyleConfig(bold = false, italic = false, underline = false)),
        identifier =
          SyntaxElementConfig("#FFFFFF", Some("#0B0F14"), StyleConfig(bold = false, italic = false, underline = false))
      )
    )

    val themeEither = ConfigurableThemeManager.configToTheme(themeConfig)

    themeEither shouldBe a[Right[?, ?]]
    val theme = themeEither.toOption.get

    theme.name shouldBe "test"
    theme.foreground shouldBe a[java.awt.Color]
    theme.syntaxColors should contain key com.serenity.ui.theme.SyntaxElement.Keyword
    theme.highlighted.background shouldBe a[java.awt.Color]

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
          ui {
            foreground = "$foregroundColor"
            background = "#0B0F14"
            cursor = "#F4D03F"
            border = "#2F3B4A"
            muted = "#7B8794"
            placeholder = "#52606D"
            highlighted { foreground = "#0B0F14", background = "#5DADE2", style { bold = false, italic = false, underline = false } }
            menu-item { foreground = "#F5F7FA", background = "#1F2933", style { bold = false, italic = false, underline = false } }
            panel { foreground = "#F5F7FA", background = "#111821", style { bold = false, italic = false, underline = false } }
            error { foreground = "#FF6B6B", background = "#2B1215", style { bold = false, italic = false, underline = false } }
          }
          syntax {
            keyword {
              foreground = "#5DADE2"
              style { bold = true, italic = false, underline = false }
            }
            string {
              foreground = "#58D68D" 
              style { bold = false, italic = false, underline = false }
            }
            comment {
              foreground = "#808080"
              style { bold = false, italic = true, underline = false }
            }
            number {
              foreground = "#F39C12"
              style { bold = false, italic = false, underline = false }
            }
            operator {
              foreground = "#E74C3C"
              style { bold = false, italic = false, underline = false }
            }
            identifier {
              foreground = "#FFFFFF"
              style { bold = false, italic = false, underline = false }
            }
            typ {
              foreground = "#AF7AC5"
              style { bold = true, italic = false, underline = false }
            }
            delimiter {
              foreground = "#D5D8DC"
              style { bold = false, italic = false, underline = false }
            }
            whitespace {
              foreground = "#0B0F14"
              style { bold = false, italic = false, underline = false }
            }
            error {
              foreground = "#FF6B6B"
              style { bold = false, italic = false, underline = true }
            }
            normal {
              foreground = "#FFFFFF"
              style { bold = false, italic = false, underline = false }
            }
          }
        }
      """
      )

    try
      // Write initial config
      writeThemeConfig("#F5F7FA")

      val reloader     = new ThemeReloader()
      val initialTheme = reloader.loadAndConvertTheme(tempFile).unsafeRunSync()
      initialTheme.ui.foreground shouldBe "#F5F7FA"

      // Update config file
      writeThemeConfig("#F4D03F")

      // Reload
      val reloadedTheme = reloader.loadAndConvertTheme(tempFile).unsafeRunSync()
      reloadedTheme.ui.foreground shouldBe "#F4D03F"

    finally Files.deleteIfExists(tempFile)
  }
