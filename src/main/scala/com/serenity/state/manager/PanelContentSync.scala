package com.serenity.state.manager

import com.serenity.lsp.config.LanguageId
import com.serenity.lsp.model.{Diagnostic as LspDiagnostic, DiagnosticSeverity as LspSeverity}
import com.serenity.spellcheck.SpellChecker
import com.serenity.state.models.*
import com.serenity.ui.layout.{Diagnostic, DiagnosticSeverity, Location, Symbol}

/** Keeps docked panels that derive from the active document -- outline, comments, diagnostics, markdown preview -- in
  * step with it. Runs on every commit, so no event source has to remember to refresh them, and recomputes a panel only
  * when something it derives from actually changed: a panel's stored content is otherwise left exactly as it was.
  *
  * Follows the active editor pane rather than the focused one, so focusing a panel doesn't empty it. An outline is
  * re-parsed synchronously only when the active document switches; an edit instead makes [[outlineRefreshDue]] ask for
  * a debounced re-parse, since a novel-length document takes around 10ms to parse -- too long for every keystroke.
  */
private[manager] object PanelContentSync:

  def synced(state: AppState, previous: AppState): AppState =
    val source         = state.activeBuffer
    val previousSource = previous.activeBuffer
    val updated = state.runtime.uiSurfaces.map {
      case surface @ UiSurface(_, content, SurfacePresentation.Docked, _) =>
        refreshed(content, state, source, previous, previousSource).fold(surface)(next => surface.copy(content = next))
      case surface => surface
    }
    if updated.corresponds(state.runtime.uiSurfaces)(_ eq _) then state
    else state.copy(runtime = state.runtime.copy(uiSurfaces = updated))

  /** The active buffer, when a docked outline shows it and it changed without switching. */
  def outlineRefreshDue(state: AppState, previous: AppState): Option[BufferId] =
    val source = state.activeBuffer
    source
      .filter(_ => hasDockedOutline(state) && !sourceSwitched(source, previous.activeBuffer))
      .filter(current => previous.activeBuffer.exists(prior => documentChanged(current, prior)))
      .map(_.id)

  /** Applies a debounced re-parse, unless the buffer is no longer active or has been edited since it was parsed. */
  def withRefreshedOutline(state: AppState, bufferId: BufferId, contentVersion: Long, symbols: List[Symbol]): AppState =
    val current =
      state.activeBuffer.exists(buffer => buffer.id == bufferId && buffer.document.contentVersion == contentVersion)
    if !current then state
    else
      val updated = state.runtime.uiSurfaces.map {
        case surface @ UiSurface(_, SurfaceContent.Outline(existing, _), SurfacePresentation.Docked, _)
            if existing != symbols =>
          surface.copy(content = SurfaceContent.Outline(symbols))
        case surface => surface
      }
      if updated.corresponds(state.runtime.uiSurfaces)(_ eq _) then state
      else state.copy(runtime = state.runtime.copy(uiSurfaces = updated))

  def outlineContent(source: Option[Buffer]): SurfaceContent.Outline =
    SurfaceContent.Outline(source.map(PanelSymbolLookup.outlineSymbolsForBuffer).getOrElse(Nil))

  def commentsContent(source: Option[Buffer]): SurfaceContent.Comments =
    SurfaceContent.Comments(source.map(PanelSymbolLookup.commentSymbolsForBuffer).getOrElse(Nil))

  def diagnosticsContent(state: AppState, source: Option[Buffer]): SurfaceContent.Diagnostics =
    SurfaceContent.Diagnostics(sourceDiagnostics(state, source).map(asPanelDiagnostic))

  def markdownPreviewContent(buffer: Buffer): SurfaceContent.MarkdownPreview =
    val title = buffer.document.filePath.flatMap(path => Option(path.getFileName).map(_.toString)).getOrElse("Untitled")
    SurfaceContent.MarkdownPreview(buffer.id, title)

  private def refreshed(
    content: SurfaceContent,
    state: AppState,
    source: Option[Buffer],
    previous: AppState,
    previousSource: Option[Buffer]
  ): Option[SurfaceContent] =
    content match
      case SurfaceContent.Outline(_, _) if sourceSwitched(source, previousSource) =>
        Some(outlineContent(source))
      case SurfaceContent.Comments(_, _) if commentsChanged(source, previousSource) =>
        Some(commentsContent(source))
      case SurfaceContent.Diagnostics(_, _)
          if sourceSwitched(source, previousSource) ||
            !(sourceDiagnostics(state, source) eq sourceDiagnostics(previous, source)) =>
        Some(diagnosticsContent(state, source))
      case SurfaceContent.MarkdownPreview(bufferId, _) =>
        source
          .filter(buffer => buffer.id != bufferId && buffer.document.language.contains(LanguageId.Markdown))
          .map(markdownPreviewContent)
      case _ => None

  private def sourceSwitched(source: Option[Buffer], previousSource: Option[Buffer]): Boolean =
    source.map(_.id) != previousSource.map(_.id)

  private def hasDockedOutline(state: AppState): Boolean =
    state.runtime.uiSurfaces.exists {
      case UiSurface(_, SurfaceContent.Outline(_, _), SurfacePresentation.Docked, _) => true
      case _                                                                         => false
    }

  private def documentChanged(current: Buffer, prior: Buffer): Boolean =
    !(current.document.content eq prior.document.content) ||
      current.document.language != prior.document.language ||
      !(current.richText eq prior.richText) ||
      !(current.annotations eq prior.annotations)

  private def commentsChanged(source: Option[Buffer], previousSource: Option[Buffer]): Boolean =
    sourceSwitched(source, previousSource) || (source, previousSource).match
      case (Some(current), Some(prior)) =>
        !(current.annotations.documentComments eq prior.annotations.documentComments)
      case _ => false

  private def sourceDiagnostics(state: AppState, source: Option[Buffer]): List[LspDiagnostic] =
    source
      .flatMap(buffer =>
        state.runtime.languageService.diagnosticsState.diagnostics.get(SpellChecker.diagnosticsUri(buffer))
      )
      .getOrElse(Nil)

  private def asPanelDiagnostic(diagnostic: LspDiagnostic): Diagnostic =
    Diagnostic(
      message = diagnostic.message,
      severity = diagnostic.severity.fold(DiagnosticSeverity.Error)(asPanelSeverity),
      location = Location(diagnostic.range.start.line, diagnostic.range.start.character)
    )

  // LSP says a diagnostic with no severity is for the client to interpret; an error is the conservative reading.
  private def asPanelSeverity(severity: LspSeverity): DiagnosticSeverity =
    severity match
      case LspSeverity.Error       => DiagnosticSeverity.Error
      case LspSeverity.Warning     => DiagnosticSeverity.Warning
      case LspSeverity.Information => DiagnosticSeverity.Info
      case LspSeverity.Hint        => DiagnosticSeverity.Hint
