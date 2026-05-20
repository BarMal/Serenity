package com.serenity

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.ui.theme.config.*
import com.serenity.ui.theme.SyntaxElement
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BundledThemesSpec extends AnyFlatSpec with Matchers:

  "Bundled dark theme" should "load successfully from resources" in {
    val manager = new ConfigurableThemeManager(new ThemeConfigLoader())
    
    val darkTheme = manager.loadThemeFromResource("themes/dark.conf").unsafeRunSync()
    
    darkTheme.name shouldBe "dark"
    darkTheme.foregroundColor.toString should include("WHITE")
    darkTheme.backgroundColor.toString should include("BLACK")
    
    // Verify syntax highlighting colors are configured
    val keywordColor = darkTheme.colorFor(SyntaxElement.Keyword)
    keywordColor.style.isBold shouldBe true
    
    val commentColor = darkTheme.colorFor(SyntaxElement.Comment) 
    commentColor.style.isItalic shouldBe true
    
    val errorColor = darkTheme.colorFor(SyntaxElement.Error)
    errorColor.style.isUnderlined shouldBe true
  }

  "Bundled light theme" should "load successfully from resources" in {
    val manager = new ConfigurableThemeManager(new ThemeConfigLoader())
    
    val lightTheme = manager.loadThemeFromResource("themes/light.conf").unsafeRunSync()
    
    lightTheme.name shouldBe "light"
    lightTheme.foregroundColor.toString should include("BLACK")
    lightTheme.backgroundColor.toString should include("WHITE")
    
    // Verify syntax highlighting colors are configured
    val keywordColor = lightTheme.colorFor(SyntaxElement.Keyword)
    keywordColor.style.isBold shouldBe true
    
    val commentColor = lightTheme.colorFor(SyntaxElement.Comment)
    commentColor.style.isItalic shouldBe true
    
    val errorColor = lightTheme.colorFor(SyntaxElement.Error)
    errorColor.style.isUnderlined shouldBe true
  }

  "Hex color parsing" should "work with theme configurations" in {
    val configString = """
      theme {
        name = "hex-test"
        colors {
          foreground = "#FFFFFF"
          background = "#000000"
          cursor = "#FF0000"
        }
        syntax {
          keyword {
            foreground = "#0066CC"
            style { bold = true, italic = false, underline = false }
          }
          string {
            foreground = "#00AA00"
            style { bold = false, italic = false, underline = false }
          }
          comment {
            foreground = "#666666"
            style { bold = false, italic = true, underline = false }
          }
          number {
            foreground = "#FF6600"
            style { bold = false, italic = false, underline = false }
          }
          operator {
            foreground = "#AA0000"
            style { bold = false, italic = false, underline = false }
          }
          identifier {
            foreground = "#000000"
            style { bold = false, italic = false, underline = false }
          }
        }
      }
    """
    
    val loader = new ThemeConfigLoader()
    val config = loader.loadThemeFromString(configString).unsafeRunSync()
    val themeResult = ConfigurableThemeManager.configToTheme(config)
    
    themeResult shouldBe a[Right[_, _]]
    val theme = themeResult.toOption.get
    
    theme.name shouldBe "hex-test"
    // The exact RGB values should be preserved
    theme.foregroundColor shouldNot be(null)
    theme.backgroundColor shouldNot be(null)
  }