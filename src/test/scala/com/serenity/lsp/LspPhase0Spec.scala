package com.serenity.lsp

import com.serenity.lsp.config.*
import com.serenity.lsp.model.*
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LspPhase0Spec extends AnyFlatSpec with Matchers with OptionValues:

  "LanguageId" should "resolve known ids case-insensitively" in {
    LanguageId.fromString("scala") shouldBe Some(LanguageId.Scala)
    LanguageId.fromString("SCALA") shouldBe Some(LanguageId.Scala)
    LanguageId.fromString("typescript") shouldBe Some(LanguageId.TypeScript)
    LanguageId.fromString("unknown") shouldBe None
  }

  "DiagnosticSeverity" should "round-trip through its integer code" in {
    DiagnosticSeverity.fromCode(1) shouldBe Some(DiagnosticSeverity.Error)
    DiagnosticSeverity.fromCode(2) shouldBe Some(DiagnosticSeverity.Warning)
    DiagnosticSeverity.fromCode(3) shouldBe Some(DiagnosticSeverity.Information)
    DiagnosticSeverity.fromCode(4) shouldBe Some(DiagnosticSeverity.Hint)
    DiagnosticSeverity.fromCode(5) shouldBe None
  }

  "FileExtension" should "map extensions to language ids" in {
    FileExtension.languageIdFor("scala") shouldBe Some(LanguageId.Scala)
    FileExtension.languageIdFor(".sbt") shouldBe Some(LanguageId.Scala)
    FileExtension.languageIdFor("rs") shouldBe Some(LanguageId.Rust)
    FileExtension.languageIdFor("ts") shouldBe Some(LanguageId.TypeScript)
    FileExtension.languageIdFor("xyz") shouldBe None
  }

  it should "enumerate all extensions for a given language" in {
    val scalaExts = FileExtension.forLanguage(LanguageId.Scala).map(_.ext)
    scalaExts should contain allOf ("scala", "sbt", "sc")
  }

  "RootMarker" should "list markers for Scala" in {
    val markers = RootMarker.forLanguage(LanguageId.Scala).map(_.filename)
    markers should contain allOf ("build.sbt", ".metals")
  }

  it should "resolve a marker by filename" in {
    RootMarker.fromFilename("build.sbt") shouldBe Some(RootMarker.BuildSbt)
    RootMarker.fromFilename("unknown") shouldBe None
  }

  "LspServerBinary" should "resolve binaries for each supported language" in {
    LspServerBinary.forLanguage(LanguageId.Scala) should contain(LspServerBinary.Metals)
    LspServerBinary.forLanguage(LanguageId.Rust) should contain(LspServerBinary.RustAnalyzer)
    LspServerBinary.forLanguage(LanguageId.Go) should contain(LspServerBinary.Gopls)
  }

  it should "look up a binary by command string" in {
    LspServerBinary.fromCommand("metals") shouldBe Some(LspServerBinary.Metals)
    LspServerBinary.fromCommand("rust-analyzer") shouldBe Some(LspServerBinary.RustAnalyzer)
    LspServerBinary.fromCommand("nonexistent") shouldBe None
  }

  "LspServerRegistry.builtIn" should "have exactly one entry per language id" in {
    val languageIds = LspServerRegistry.builtIn.map(_.languageId)
    languageIds.distinct.size shouldBe languageIds.size
  }

  it should "include Scala/Metals as the first entry" in {
    LspServerRegistry.builtIn.head.languageId shouldBe LanguageId.Scala
    LspServerRegistry.builtIn.head.binary shouldBe LspServerBinary.Metals
  }

  "LspEffect" should "carry language id and uri" in {
    val effect = LspEffect.FileOpened("file:///foo/Bar.scala", LanguageId.Scala, "object Foo")
    effect match
      case LspEffect.FileOpened(uri, lang, _) =>
        uri shouldBe "file:///foo/Bar.scala"
        lang shouldBe LanguageId.Scala
      case _ => fail("wrong variant")
  }

  "LspUserConfig.empty" should "have no server overrides" in {
    LspUserConfig.empty.servers shouldBe None
  }

  it should "disable configured language servers" in {
    val userConfig = LspUserConfig(
      servers = Some(
        Map(
          LanguageId.Scala.id -> LspServerOverride(
            command = None,
            args = None,
            enabled = Some(false)
          )
        )
      )
    )

    LspServerRegistry.configuredServer(LanguageId.Scala, userConfig) shouldBe None
  }

  it should "apply configured command and args for a language server" in {
    val userConfig = LspUserConfig(
      servers = Some(
        Map(
          LanguageId.Scala.id -> LspServerOverride(
            command = Some("custom-metals"),
            args = Some(List("--stdio", "--verbose")),
            enabled = Some(true)
          )
        )
      )
    )

    val server = LspServerRegistry.configuredServer(LanguageId.Scala, userConfig).value

    server.languageId shouldBe LanguageId.Scala
    server.command shouldBe "custom-metals"
    server.defaultArgs shouldBe List("--stdio", "--verbose")
  }

  "Diagnostic" should "hold range, severity and message" in {
    val pos   = LspPosition(10, 5)
    val range = LspRange(pos, pos.copy(character = 20))
    val diag  = Diagnostic(range, Some(DiagnosticSeverity.Error), "type mismatch")
    diag.severity shouldBe Some(DiagnosticSeverity.Error)
    diag.message shouldBe "type mismatch"
    diag.range.start.line shouldBe 10
  }
