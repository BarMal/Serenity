package com.serenity.state.manager

import java.nio.file.{Files, Path}

import scala.concurrent.duration.*

import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import com.serenity.command.*
import com.serenity.config.{AppConfig, AppMode, ConfigManager}
import com.serenity.keystroke.events.{InsertChar, LspEvent}
import com.serenity.lsp.LspEffect
import com.serenity.lsp.config.LanguageId
import com.serenity.lsp.model.{Diagnostic, DiagnosticSeverity, LspPosition, LspRange}
import com.serenity.state.manager.StateManagerTestFacade.*
import com.serenity.state.models.*
import com.serenity.testkit.SharedDictionary
import com.serenity.{StateManagerTestSupport, TestTemp}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Leaving code mode lets the language servers go, and entering it brings them back (#1294): the mode switch is one
  * lifecycle transition, whichever way the app mode changes -- the command, a workflow preset, or the config file being
  * edited from outside.
  */
class ModeSwitchLifecycleSpec extends AnyFlatSpec with Matchers with StateManagerTestSupport:

  /** A running editor whose LSP lane is read from the outside, as `LspManager` reads it. */
  final private class Editor(val manager: StateManager, val directory: Path):

    def state: AppState = manager.getCurrentState.unsafeRunSync()

    def switchTo(mode: AppMode): Unit =
      manager
        .executeCommand(
          Command.typed(
            s"app-mode-${mode.configKey}",
            s"Switch to ${mode.configKey} mode",
            CommandIntent.View(ViewIntent.SetAppMode(mode)),
            CommandCategory.Settings
          )
        )
        .unsafeRunSync()

    def effects(count: Int, within: FiniteDuration = 3.seconds): List[String] =
      manager.lspEffectSource.lspEffectStream
        .take(count.toLong)
        .interruptAfter(within)
        .compile
        .toList
        .unsafeRunSync()
        .map(describe)

    def quiet: List[String] = effects(1, 500.millis)

    def uri(name: String): String = directory.resolve(name).toUri.toString

    /** The editor's first buffer, made into a document. */
    def firstDocument(name: String, language: LanguageId, content: String): BufferId =
      val id = BufferId(0)
      manager.updateBuffer(id, content).unsafeRunSync()
      manager.setBufferFilePath(id, directory.resolve(name)).unsafeRunSync()
      manager.updateStateValidated(withLanguage(id, Some(language))).unsafeRunSync()
      id

    def document(name: String, language: Option[LanguageId], content: String, onDisk: Boolean = true): BufferId =
      val id = manager.createBuffer(content, Option.when(onDisk)(directory.resolve(name))).unsafeRunSync()
      manager.updateStateValidated(withLanguage(id, language)).unsafeRunSync()
      id

    def reloadConfig(mode: AppMode): Unit =
      Files.writeString(
        directory.resolve("config.conf"),
        ConfigManager.configToString(AppConfig.default.withAppMode(mode))
      )
      (manager.fileService.configWatch.traverse_(_.reload) >> manager.runtimeLifecycle.awaitEffects).unsafeRunSync()

  private def describe(effect: LspEffect): String =
    effect match
      case LspEffect.FileOpened(uri, language, text)              => s"opened $uri as ${language.id}: $text"
      case LspEffect.FileChanged(uri, language, text, version, _) => s"changed $uri as ${language.id} v$version: $text"
      case LspEffect.FileClosed(uri, language)                    => s"closed $uri as ${language.id}"
      case other                                                  => other.toString

  private def withLanguage(id: BufferId, language: Option[LanguageId]): AppState => AppState = state =>
    state.copy(persisted =
      state.persisted.copy(buffers =
        state.persisted.buffers.updatedWith(id)(
          _.map(buffer => buffer.copy(document = buffer.document.copy(language = language)))
        )
      )
    )

  private def editor(mode: AppMode = AppMode.Code, withConfigFile: Boolean = false): Editor =
    val directory = TestTemp.directory("mode-switch-lifecycle")
    val config    = AppConfig.default.withAppMode(mode)
    val manager = StateManager
      .apply(
        testLogger("ModeSwitchLifecycleSpec"),
        sessionRootOverride = Some(directory.resolve("session")),
        initialConfig = config,
        configPersistencePath = Option.when(withConfigFile)(directory.resolve("config.conf")),
        dictionaryCache = SharedDictionary.cacheFor(config)
      )
      .unsafeRunSync()
    Editor(manager, directory)

  private def twoDocuments(editor: Editor): Unit =
    editor.firstDocument("A.scala", LanguageId.Scala, "object A")
    editor.document("B.py", Some(LanguageId.Python), "print(1)")
    editor.document("notes.txt", None, "plain notes")
    editor.document("Scratch.scala", Some(LanguageId.Scala), "unsaved scratch", onDisk = false)

  "Switching from code to prose" should "close every open document that has a language, then release the servers" in {
    val editor = this.editor()
    twoDocuments(editor)

    editor.switchTo(AppMode.Prose)

    editor.effects(3) shouldBe List(
      s"closed ${editor.uri("A.scala")} as scala",
      s"closed ${editor.uri("B.py")} as python",
      "ReleaseAll"
    )
    editor.quiet shouldBe Nil
  }

  it should "leave no language-server data on the documents it closed" in {
    val editor = this.editor()
    val id     = editor.firstDocument("A.scala", LanguageId.Scala, "object A")
    editor.manager.updateStateValidated(seededWithServerData(id)).unsafeRunSync()

    editor.switchTo(AppMode.Prose)

    val languageService = editor.state.runtime.languageService
    languageService.diagnosticsState.diagnostics.values.flatten.filter(_.source.contains("metals")) shouldBe Nil
    languageService.semanticTokensState.unavailableUris shouldBe Set.empty
    languageService.progress shouldBe Map.empty
  }

  it should "ignore what a server reports after the switch, and take it again in code mode" in {
    val editor = this.editor()
    val id     = editor.firstDocument("A.scala", LanguageId.Scala, "object A")
    val report = LspEvent.LspDiagnosticsReceived(editor.uri("A.scala"), List(serverComplaint))
    def reported: List[Diagnostic] =
      val state = editor.state
      state.runtime.languageService.diagnosticsState.diagnostics
        .getOrElse(state.runtime.bufferIndexMemos.uriFor(state.persisted.buffers(id)), Nil)
        .filter(_.source.contains("metals"))

    editor.switchTo(AppMode.Prose)
    editor.manager.applyEvent(report).unsafeRunSync()
    val whileInProse = reported
    editor.switchTo(AppMode.Code)
    editor.manager.applyEvent(report).unsafeRunSync()

    whileInProse shouldBe Nil
    reported shouldBe List(serverComplaint)
  }

  it should "issue no language-server request while the workspace is in prose" in {
    val editor = this.editor()
    editor.firstDocument("A.scala", LanguageId.Scala, "object A")
    val hover = Command.typed("lsp-hover", "Hover", CommandIntent.Lsp(LspIntent.RequestLspHover), CommandCategory.Edit)
    editor.manager.executeCommand(hover).unsafeRunSync()
    val inCode = editor.effects(1).map(_.takeWhile(_ != '('))
    editor.switchTo(AppMode.Prose)
    editor.effects(2)

    editor.manager.executeCommand(hover).unsafeRunSync()

    inCode shouldBe List("HoverRequested")
    editor.quiet shouldBe Nil
  }

  "Switching from prose to code" should "open every buffer that has a language and a file, with its current text" in {
    val editor = this.editor(AppMode.Prose)
    twoDocuments(editor)
    editor.quiet shouldBe Nil

    editor.switchTo(AppMode.Code)

    editor.effects(2) shouldBe List(
      s"opened ${editor.uri("A.scala")} as scala: object A",
      s"opened ${editor.uri("B.py")} as python: print(1)"
    )
    editor.quiet shouldBe Nil
  }

  "Switching from code to prose and back" should "leave the servers in sync, versioning edits from the reopen" in {
    val editor = this.editor()
    editor.firstDocument("A.scala", LanguageId.Scala, "object A")

    editor.switchTo(AppMode.Prose)
    editor.switchTo(AppMode.Code)
    editor.manager.applyEvent(InsertChar('x')).unsafeRunSync()

    val edited = editor.state.persisted.buffers(BufferId(0)).document.content
    editor.effects(4) shouldBe List(
      s"closed ${editor.uri("A.scala")} as scala",
      "ReleaseAll",
      s"opened ${editor.uri("A.scala")} as scala: object A",
      s"changed ${editor.uri("A.scala")} as scala v2: $edited"
    )
  }

  "The config file changing the app mode" should "release the servers the same way the command does" in {
    val editor = this.editor(withConfigFile = true)
    twoDocuments(editor)

    editor.reloadConfig(AppMode.Prose)

    editor.state.persisted.config.appMode shouldBe AppMode.Prose
    editor.effects(3) shouldBe List(
      s"closed ${editor.uri("A.scala")} as scala",
      s"closed ${editor.uri("B.py")} as python",
      "ReleaseAll"
    )
  }

  it should "open the documents again when it puts the workspace back in code mode" in {
    val editor = this.editor(AppMode.Prose, withConfigFile = true)
    twoDocuments(editor)

    editor.reloadConfig(AppMode.Code)

    editor.state.persisted.config.appMode shouldBe AppMode.Code
    editor.effects(2) shouldBe List(
      s"opened ${editor.uri("A.scala")} as scala: object A",
      s"opened ${editor.uri("B.py")} as python: print(1)"
    )
  }

  "Applying a prose workflow preset" should "release the servers the same way the command does" in {
    val editor = this.editor()
    twoDocuments(editor)

    editor.manager
      .executeCommand(
        Command.typed(
          "apply-writing-preset",
          "Apply the Writing workflow",
          CommandIntent.UiPresets(UiPresetsIntent.ApplyUiPreset("Writing")),
          CommandCategory.View
        )
      )
      .unsafeRunSync()
    editor.manager.runtimeLifecycle.awaitEffects.unsafeRunSync()

    editor.state.persisted.config.appMode shouldBe AppMode.Prose
    editor.effects(3) shouldBe List(
      s"closed ${editor.uri("A.scala")} as scala",
      s"closed ${editor.uri("B.py")} as python",
      "ReleaseAll"
    )
  }

  private val serverComplaint =
    Diagnostic(
      LspRange(LspPosition(0, 0), LspPosition(0, 3)),
      Some(DiagnosticSeverity.Error),
      "type mismatch",
      Some("metals")
    )

  private def seededWithServerData(id: BufferId): AppState => AppState = state =>
    val uri             = state.runtime.bufferIndexMemos.uriFor(state.persisted.buffers(id))
    val languageService = state.runtime.languageService
    state.copy(runtime =
      state.runtime.copy(languageService =
        languageService.copy(
          diagnosticsState = languageService.diagnosticsState.copy(diagnostics = Map(uri -> List(serverComplaint))),
          semanticTokensState = languageService.semanticTokensState.copy(unavailableUris = Set(uri)),
          progress = Map(LanguageId.Scala -> List(com.serenity.lsp.model.LspProgressTask("t", "Indexing", None, None)))
        )
      )
    )
