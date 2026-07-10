package com.serenity

import com.serenity.config.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DocumentConfigSpec extends AnyFlatSpec with Matchers:

  "DocumentConfig" should "own document-mode and markdown-view schema metadata" in {
    DocumentConfig.Schema.currentKeys.should(
      contain allOf (
        "document.default_mode",
        "document.default.mode",
        "document.markdown_view",
        "document.markdown.view"
      )
    )

    DocumentConfig.Schema.deprecatedKeys.should(
      contain allOf (
        "document_default_mode"  -> "document.default_mode",
        "document_markdown_view" -> "document.markdown_view"
      )
    )
  }

  it should "group markdown view and default document mode under AppConfig" in {
    val config = AppConfig.default
      .withMarkdownViewMode(MarkdownViewMode.InlineLens)
      .withDefaultDocumentMode(DefaultDocumentMode.RichText)

    config.documentConfig.shouldBe(
      DocumentConfig(
        markdownViewMode = MarkdownViewMode.InlineLens,
        defaultMode = DefaultDocumentMode.RichText
      )
    )
  }

  it should "parse document config values centrally" in {
    MarkdownViewMode.fromConfigKey("preview").shouldBe(Some(MarkdownViewMode.SplitPreview))
    DefaultDocumentMode.fromConfigKey("rtf").shouldBe(Some(DefaultDocumentMode.RichText))
    MarkdownViewMode.fromConfigKey("unknown").shouldBe(None)
  }
