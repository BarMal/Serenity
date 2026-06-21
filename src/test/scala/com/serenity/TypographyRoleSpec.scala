package com.serenity

import java.awt.Font

import com.serenity.lsp.config.LanguageId
import com.serenity.rope.Balance
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.layout.{LayoutEngine, ViewportSize}
import com.serenity.ui.renderer.RenderContext
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TypographyRoleSpec extends AnyFlatSpec with Matchers:

  given Balance = Balance.default

  private val codeFont = Font(Font.MONOSPACED, Font.PLAIN, 12)
  private val textFont = Font(Font.SERIF, Font.PLAIN, 14)
  private val uiFont   = Font(Font.SANS_SERIF, Font.PLAIN, 16)

  "TypographyRole" should "classify buffers by content semantics" in {
    Buffer.fromString(BufferId(1), "plain").typographyRole shouldBe TypographyRole.Prose
    Buffer.fromString(BufferId(2), "# title").copy(language = Some(LanguageId.Markdown)).typographyRole shouldBe
      TypographyRole.MarkdownSource
    Buffer.fromString(BufferId(3), "object Main").copy(language = Some(LanguageId.Scala)).typographyRole shouldBe
      TypographyRole.Code
  }

  it should "choose runtime fonts by semantic role" in {
    val context = RenderContext(
      surface = new MockRenderSurface(20, 10),
      layout = LayoutEngine.calculateLayout(AppState.initial, ViewportSize(20, 10)),
      codeFont = codeFont,
      textFont = textFont,
      uiFont = uiFont,
      cellMetrics = com.serenity.ui.layout.CellMetrics.fromFont(codeFont),
      uiMetrics = com.serenity.ui.layout.CellMetrics.fromFont(uiFont)
    )

    context.fontForRole(TypographyRole.Code) shouldBe codeFont
    context.fontForRole(TypographyRole.Prose) shouldBe textFont
    context.fontForRole(TypographyRole.MarkdownSource) shouldBe textFont
    context.fontForRole(TypographyRole.Ui) shouldBe uiFont
  }

  it should "choose preview fonts by semantic role" in {
    val config = FontConfig(
      codeFontFamily = Font.MONOSPACED,
      textFontFamily = Font.SERIF,
      uiFontFamily = Font.SANS_SERIF,
      fontSize = 13.0f,
      textFontSize = 15.0f,
      uiFontSize = 17.0f
    )

    FontLoader.previewFontForRole(config, TypographyRole.Code).getSize2D shouldBe 13.0f
    FontLoader.previewFontForRole(config, TypographyRole.Prose).getSize2D shouldBe 15.0f
    FontLoader.previewFontForRole(config, TypographyRole.MarkdownSource).getSize2D shouldBe 15.0f
    FontLoader.previewFontForRole(config, TypographyRole.Ui).getSize2D shouldBe 17.0f
  }

  it should "apply the configured text scale multiplier to every preview font role" in {
    val config = FontConfig(
      codeFontFamily = Font.MONOSPACED,
      textFontFamily = Font.SERIF,
      uiFontFamily = Font.SANS_SERIF,
      fontSize = 13.0f,
      textFontSize = 15.0f,
      uiFontSize = 17.0f,
      textScaleMultiplier = 2.0
    )

    FontLoader.previewFontForRole(config, TypographyRole.Code).getSize2D shouldBe 26.0f
    FontLoader.previewFontForRole(config, TypographyRole.Prose).getSize2D shouldBe 30.0f
    FontLoader.previewFontForRole(config, TypographyRole.MarkdownSource).getSize2D shouldBe 30.0f
    FontLoader.previewFontForRole(config, TypographyRole.Ui).getSize2D shouldBe 34.0f
  }
end TypographyRoleSpec
