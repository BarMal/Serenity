package com.serenity.ui.fonts

import java.awt.Font

import com.serenity.ui.theme.TextStyle
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RoleFontResolutionSpec extends AnyFlatSpec with Matchers:

  private val toPhysical: String => String = {
    case "SansSerif"  => "Physical Sans"
    case "Monospaced" => "Physical Mono"
    case other        => other
  }

  "FontLoader.previewTextFont" should "build the prose font in the resolved family" in {
    val config = FontLoader.FontConfig(textFontFamily = Font.SANS_SERIF)
    FontLoader.previewTextFont(config, toPhysical).getName shouldBe "Physical Sans"
  }

  "FontLoader.previewUiFont" should "build the UI font in the resolved family" in {
    val config = FontLoader.FontConfig(uiFontFamily = Font.SANS_SERIF)
    FontLoader.previewUiFont(config, toPhysical).getName shouldBe "Physical Sans"
  }

  "FontLoader.previewCodeFont" should "resolve a configured logical code family but leave the bundled one alone" in {
    FontLoader
      .previewCodeFont(FontLoader.FontConfig(codeFontFamily = Font.MONOSPACED), toPhysical)
      .getName shouldBe "Physical Mono"
    val bundled = FontLoader.previewCodeFont(FontLoader.FontConfig(), _ => "never asked")
    bundled.getName should not be "never asked"
  }

  it should "leave a family the user named alone" in {
    FontLoader
      .previewTextFont(FontLoader.FontConfig(textFontFamily = "Georgia"), toPhysical)
      .getName shouldBe "Georgia"
  }

  "TextStyle.styledFont" should "resolve the family a style overrides the base font with" in {
    val base  = new Font("Dialog", Font.PLAIN, 12)
    val style = TextStyle.normal.copy(fontFamily = Some(Font.MONOSPACED))
    TextStyle.styledFont(base, style, toPhysical).getName shouldBe "Physical Mono"
  }

  it should "keep the base font when a style names no family" in {
    val base = new Font("Dialog", Font.PLAIN, 12)
    TextStyle.styledFont(base, TextStyle.normal, _ => "never asked").getFamily shouldBe base.getFamily
  }

  "FontFallback" should "keep the composite Dialog font as its last resort, unresolved" in {
    FontFallback(Vector("Georgia"), None).slotFamilies.last shouldBe Font.DIALOG
  }
