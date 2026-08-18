package com.serenity.app

import java.awt.Font
import java.util.concurrent.atomic.AtomicReference

import cats.effect.IO
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.layout.CellMetrics
import org.typelevel.log4cats.Logger

final class RuntimeDisplayState private (
    codeFontRef: AtomicReference[Font],
    textFontRef: AtomicReference[Font],
    uiFontRef: AtomicReference[Font],
    codeMetricsRef: AtomicReference[CellMetrics],
    textMetricsRef: AtomicReference[CellMetrics],
    uiMetricsRef: AtomicReference[CellMetrics]
):

  def codeFont: Font =
    codeFontRef.get()

  def textFont: Font =
    textFontRef.get()

  def uiFont: Font =
    uiFontRef.get()

  def codeMetrics: CellMetrics =
    codeMetricsRef.get()

  def textMetrics: CellMetrics =
    textMetricsRef.get()

  def uiMetrics: CellMetrics =
    uiMetricsRef.get()

  def primaryMetrics: CellMetrics =
    codeMetricsRef.get()

  def update(config: FontConfig)(using logger: Logger[IO]): IO[Unit] =
    RuntimeDisplayState
      .load(config)
      .flatMap { snapshot =>
        if snapshot.codeMetrics.isValid && snapshot.textMetrics.isValid && snapshot.uiMetrics.isValid then
          IO {
            codeFontRef.set(snapshot.codeFont)
            textFontRef.set(snapshot.textFont)
            uiFontRef.set(snapshot.uiFont)
            codeMetricsRef.set(snapshot.codeMetrics)
            textMetricsRef.set(snapshot.textMetrics)
            uiMetricsRef.set(snapshot.uiMetrics)
          }
        else
          logger.warn(
            "Rejecting font config: invalid metrics " +
              s"(code charWidth=${snapshot.codeMetrics.charWidth}, text charWidth=${snapshot.textMetrics.charWidth}, ui charWidth=${snapshot.uiMetrics.charWidth}). " +
              "Keeping previous fonts."
          )
      }

object RuntimeDisplayState:

  final private case class Snapshot(
      codeFont: Font,
      textFont: Font,
      uiFont: Font,
      codeMetrics: CellMetrics,
      textMetrics: CellMetrics,
      uiMetrics: CellMetrics
  )

  def create(config: FontConfig)(using logger: Logger[IO]): IO[RuntimeDisplayState] =
    load(config).map { snapshot =>
      new RuntimeDisplayState(
        new AtomicReference(snapshot.codeFont),
        new AtomicReference(snapshot.textFont),
        new AtomicReference(snapshot.uiFont),
        new AtomicReference(snapshot.codeMetrics),
        new AtomicReference(snapshot.textMetrics),
        new AtomicReference(snapshot.uiMetrics)
      )
    }

  private def load(config: FontConfig)(using logger: Logger[IO]): IO[Snapshot] =
    for
      codeFont <- FontLoader.loadCodeFont(config)
      textFont <- FontLoader.loadTextFont(config)
      uiFont   <- FontLoader.loadUiFont(config)
    yield Snapshot(
      codeFont = codeFont,
      textFont = textFont,
      uiFont = uiFont,
      codeMetrics = CellMetrics.fromFont(codeFont),
      textMetrics = CellMetrics.fromFont(textFont),
      uiMetrics = CellMetrics.fromFont(uiFont)
    )
