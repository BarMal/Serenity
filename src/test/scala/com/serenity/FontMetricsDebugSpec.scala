package com.serenity

import java.awt.font.FontRenderContext
import java.awt.geom.AffineTransform
import java.awt.Font

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.serenity.state.manager.StateManager
import com.serenity.testkit.SharedDictionary
import com.serenity.ui.fonts.{FontLoader, LogicalFontFamilies}
import com.serenity.ui.layout.CellMetrics
import org.scalatest.flatspec.AnyFlatSpec
import org.typelevel.log4cats.slf4j.Slf4jFactory
import org.typelevel.log4cats.{LoggerFactory, LoggerName}

class FontMetricsDebugSpec extends AnyFlatSpec:

  private val context = FontRenderContext(AffineTransform(), true, true)
  private val probe   = "Buffer 1 - unsaved Hamburgefonstiv"

  private def line(label: String, font: Font): String =
    val metrics = font.getLineMetrics(probe, context)
    val cell    = CellMetrics.fromFont(font)
    f"DIAGFONT $label%-28s family=${font.getFamily}%-18s name=${font.getName}%-18s ascent=${metrics.getAscent}%.3f descent=${metrics.getDescent}%.3f leading=${metrics.getLeading}%.3f height=${metrics.getHeight}%.3f width=${font.getStringBounds(probe, context).getWidth}%.3f cell=(${cell.charWidth},${cell.lineHeight},${cell.ascent})"

  "The logical fonts and what they resolve to" should "be described" in {
    println(s"DIAGENV os=${System.getProperty("os.name")} noLogicalFonts=${java.lang.Boolean.getBoolean("serenity.debug.noLogicalFonts")}")
    List(Font.SANS_SERIF, Font.SERIF, Font.MONOSPACED, Font.DIALOG, Font.DIALOG_INPUT).foreach { logical =>
      val resolved = LogicalFontFamilies.system.resolve(logical)
      println(line(s"$logical (logical)", new Font(logical, Font.PLAIN, 12)))
      println(line(s"$logical -> $resolved", new Font(resolved, Font.PLAIN, 12)))
    }
    val config = FontLoader.FontConfig()
    println(line("previewTextFont default", FontLoader.previewTextFont(config)))
    println(line("previewUiFont default", FontLoader.previewUiFont(config)))
    println(line("previewCodeFont default", FontLoader.previewCodeFont(config)))
    given LoggerFactory[IO] = Slf4jFactory.create[IO]
    val logger              = LoggerFactory[IO].getLogger(using LoggerName("Test"))
    val stateManager = StateManager
      .apply(logger, dictionaryCache = SharedDictionary.default)(using com.serenity.rope.Balance.default, LoggerFactory[IO])
      .unsafeRunSync()
    val state = stateManager.getCurrentState.unsafeRunSync()
    val pane  = state.persisted.layout.editorPanes.values.head
    println(s"DIAGVIEWPORT visibleLines=${pane.viewport.visibleLines} visibleColumns=${pane.viewport.visibleColumns}")
    succeed
  }
