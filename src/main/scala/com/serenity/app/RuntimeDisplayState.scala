package com.serenity.app

import java.awt.Font
import java.util.concurrent.atomic.AtomicReference

import cats.effect.IO
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.fonts.FontLoader.FontConfig
import com.serenity.ui.layout.CellMetrics
import org.typelevel.log4cats.Logger

/** The fonts and cell metrics the renderer is currently drawing with.
  *
  * Held as a single reference to one immutable [[RuntimeDisplayState.Snapshot]]. The six values move together -- each
  * metric is derived from its font -- so publishing them independently would let a reader observe a new font beside a
  * stale metric and lay text out at the wrong advance. Callers that read more than one value should take [[snapshot]]
  * once and read from it, rather than calling several accessors in turn.
  */
final class RuntimeDisplayState private (
    current: AtomicReference[RuntimeDisplayState.Snapshot]
):

  /** The current fonts and metrics as one coherent value. Prefer this wherever more than one is needed. */
  def snapshot: RuntimeDisplayState.Snapshot =
    current.get()

  def codeFont: Font = snapshot.codeFont
  def textFont: Font = snapshot.textFont
  def uiFont: Font   = snapshot.uiFont

  def codeMetrics: CellMetrics = snapshot.codeMetrics
  def textMetrics: CellMetrics = snapshot.textMetrics
  def uiMetrics: CellMetrics   = snapshot.uiMetrics

  /** Metrics for the primary editor grid, which is sized by the code font. */
  def primaryMetrics: CellMetrics = snapshot.codeMetrics

  /** Load `config` and publish it as one generation, or keep the current one if its metrics are unusable. */
  def update(config: FontConfig)(using logger: Logger[IO]): IO[Unit] =
    RuntimeDisplayState
      .load(config)
      .flatMap { next =>
        if next.isValid then IO(current.set(next))
        else
          logger.warn(
            "Rejecting font config: invalid metrics " +
              s"(code charWidth=${next.codeMetrics.charWidth}, text charWidth=${next.textMetrics.charWidth}, ui charWidth=${next.uiMetrics.charWidth}). " +
              "Keeping previous fonts."
          )
      }

object RuntimeDisplayState:

  /** One generation of runtime typography: three fonts and the cell metrics derived from them. */
  final case class Snapshot(
      codeFont: Font,
      textFont: Font,
      uiFont: Font,
      codeMetrics: CellMetrics,
      textMetrics: CellMetrics,
      uiMetrics: CellMetrics
  ):
    def isValid: Boolean = codeMetrics.isValid && textMetrics.isValid && uiMetrics.isValid

  def create(config: FontConfig)(using logger: Logger[IO]): IO[RuntimeDisplayState] =
    load(config).map(snapshot => new RuntimeDisplayState(new AtomicReference(snapshot)))

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
