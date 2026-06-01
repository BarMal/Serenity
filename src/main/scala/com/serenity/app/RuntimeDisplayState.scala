package com.serenity.app

import java.awt.Font
import java.util.concurrent.atomic.AtomicReference

import cats.effect.IO
import com.serenity.lsp.config.LanguageId
import com.serenity.state.models.AppState
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.layout.CellMetrics
import org.typelevel.log4cats.Logger

final class RuntimeDisplayState private (
    codeFontRef: AtomicReference[Font],
    textFontRef: AtomicReference[Font],
    codeMetricsRef: AtomicReference[CellMetrics],
    textMetricsRef: AtomicReference[CellMetrics]
):

  def codeFont: Font =
    codeFontRef.get()

  def textFont: Font =
    textFontRef.get()

  def codeMetrics: CellMetrics =
    codeMetricsRef.get()

  def textMetrics: CellMetrics =
    textMetricsRef.get()

  def fontFor(state: AppState): Font =
    if usesChromeTypography(state) || usesCodeTypography(state) then codeFont else textFont

  def metricsFor(state: AppState): CellMetrics =
    if usesChromeTypography(state) || usesCodeTypography(state) then codeMetrics else textMetrics

  def update(config: FontConfig)(using logger: Logger[IO]): IO[Unit] =
    RuntimeDisplayState
      .load(config)
      .map { snapshot =>
        codeFontRef.set(snapshot.codeFont)
        textFontRef.set(snapshot.textFont)
        codeMetricsRef.set(snapshot.codeMetrics)
        textMetricsRef.set(snapshot.textMetrics)
      }

  private def usesCodeTypography(state: AppState): Boolean =
    state.layout.activeEditorPaneId
      .flatMap(state.layout.editorPanes.get)
      .flatMap(_.bufferId)
      .orElse(state.focusedBufferId)
      .flatMap(state.buffers.get)
      .flatMap(_.language)
      .exists(RuntimeDisplayState.usesCodeTypography)

  private def usesChromeTypography(state: AppState): Boolean =
    state.activeSurface.exists { surface =>
      surface.content match
        case com.serenity.state.models.SurfaceContent.StartPage(_)                   => true
        case com.serenity.state.models.SurfaceContent.CommandPalette(_)              => true
        case com.serenity.state.models.SurfaceContent.CommandPaletteSubmenu(_, _, _) => true
        case com.serenity.state.models.SurfaceContent.ThemePicker(_)                 => true
        case com.serenity.state.models.SurfaceContent.FileSearch(_)                  => true
        case com.serenity.state.models.SurfaceContent.ModalWorkflow(_)               => true
        case _                                                                       => false
    }

object RuntimeDisplayState:

  private case class Snapshot(
      codeFont: Font,
      textFont: Font,
      codeMetrics: CellMetrics,
      textMetrics: CellMetrics
  )

  def create(config: FontConfig)(using logger: Logger[IO]): IO[RuntimeDisplayState] =
    load(config).map { snapshot =>
      new RuntimeDisplayState(
        new AtomicReference(snapshot.codeFont),
        new AtomicReference(snapshot.textFont),
        new AtomicReference(snapshot.codeMetrics),
        new AtomicReference(snapshot.textMetrics)
      )
    }

  private def load(config: FontConfig)(using logger: Logger[IO]): IO[Snapshot] =
    for
      codeFont <- FontLoader.loadCodeFont(config)
      textFont <- FontLoader.loadTextFont(config)
    yield Snapshot(
      codeFont = codeFont,
      textFont = textFont,
      codeMetrics = CellMetrics.fromFont(codeFont),
      textMetrics = CellMetrics.fromFont(textFont)
    )

  private def usesCodeTypography(languageId: LanguageId): Boolean =
    languageId != LanguageId.Markdown
