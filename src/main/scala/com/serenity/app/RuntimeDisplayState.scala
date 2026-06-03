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

  def primaryMetrics: CellMetrics =
    codeMetricsRef.get()

  def update(config: FontConfig)(using logger: Logger[IO]): IO[Unit] =
    RuntimeDisplayState
      .load(config)
      .flatMap { snapshot =>
        if snapshot.codeMetrics.isValid && snapshot.textMetrics.isValid then
          IO {
            codeFontRef.set(snapshot.codeFont)
            textFontRef.set(snapshot.textFont)
            codeMetricsRef.set(snapshot.codeMetrics)
            textMetricsRef.set(snapshot.textMetrics)
          }
        else
          logger.warn(
            s"Rejecting font config: invalid metrics " +
              s"(code charWidth=${snapshot.codeMetrics.charWidth}, text charWidth=${snapshot.textMetrics.charWidth}). " +
              s"Keeping previous fonts."
          )
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
