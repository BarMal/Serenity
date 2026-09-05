package com.serenity

import com.serenity.config.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DocumentConfigSpec extends AnyFlatSpec with Matchers:

  "DocumentConfig" should "own document-mode and markdown-view schema metadata" in {
    ConfigKeySchema.isKnownKey("document.default_mode") shouldBe true
    ConfigKeySchema.isKnownKey("document.default.mode") shouldBe true
    ConfigKeySchema.isKnownKey("document.markdown_view") shouldBe true
    ConfigKeySchema.isKnownKey("document.markdown.view") shouldBe true

    ConfigKeySchema.deprecatedKeys.should(
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

  it should "parse document config entries centrally" in {
    val markdownConfig =
      ConfigRegistry
        .read(AppConfig.default, "document_markdown_view", "preview")
        .getOrElse(fail("markdown parse"))
    val defaultModeConfig =
      ConfigRegistry
        .read(AppConfig.default, "document.default.mode", "rtf")
        .getOrElse(fail("default mode parse"))

    markdownConfig.documentConfig.markdownViewMode.shouldBe(MarkdownViewMode.SplitPreview)
    defaultModeConfig.documentConfig.defaultMode.shouldBe(DefaultDocumentMode.RichText)
    ConfigRegistry.read(AppConfig.default, "document.default_mode", "unknown").shouldBe(None)
  }

  it should "validate document config entries centrally" in {
    ConfigRegistry.rejects("document.markdown_view", "preview").shouldBe(false)
    ConfigRegistry.rejects("document.markdown_view", "unknown").shouldBe(true)
    ConfigRegistry.rejects("document.default_mode", "rtf").shouldBe(false)
    ConfigRegistry.rejects("document.default_mode", "").shouldBe(true)
  }
