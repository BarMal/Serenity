package com.serenity.perf

import java.awt.image.BufferedImage
import java.awt.{Font, GraphicsEnvironment}
import java.nio.file.Paths
import java.util.concurrent.atomic.AtomicReference

import cats.effect.unsafe.implicits.global
import com.serenity.config.AppConfig
import com.serenity.keystroke.events.{Event, InsertChar}
import com.serenity.state.manager.{DamageProducer, RenderCaches}
import com.serenity.state.models.*
import com.serenity.ui.fonts.FontLoader
import com.serenity.ui.layout.{CellMetrics, ViewportSize}
import com.serenity.ui.renderer.{FrameOutput, Java2DRenderSurface, RendererFramePlanner, ScreenIdentity}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** #1866: a frame drawn incrementally over text that needs fallback fonts equals one drawn from scratch. The fonts are
  * physical (a logical font falls back on its own), and the fallback is a test subset font.
  */
class FontFallbackColdFrameSpec extends AnyFlatSpec with Matchers:

  given com.serenity.rope.Balance = com.serenity.rope.Balance.default

  private def physical(resource: String, sizePx: Float): Font =
    Font.createFont(Font.TRUETYPE_FONT, Paths.get(getClass.getResource(resource).toURI).toFile).deriveFont(sizePx)

  private val fallbackFamily =
    val font = Font.createFont(
      Font.TRUETYPE_FONT,
      Paths.get(getClass.getResource("/fonts/SerenityFallbackTest.ttf").toURI).toFile
    )
    val _ = GraphicsEnvironment.getLocalGraphicsEnvironment.registerFont(font)
    font.getFamily

  private val codeFont    = physical("/fonts/MonaspaceNeon-Regular.otf", 12.0f)
  private val textFont    = physical("/fonts/CourierPrime-Regular.ttf", 14.0f)
  private val uiFont      = Font(Font.SANS_SERIF, Font.PLAIN, 12)
  private val cellMetrics = CellMetrics.fromFont(codeFont)
  private val uiMetrics   = CellMetrics.fromFont(uiFont)
  private val viewport    = ViewportSize(1500 / cellMetrics.charWidth, 1000 / cellMetrics.lineHeight)

  private val plainText = Vector.tabulate(40)(line => s"line $line plain mixed text and more end").mkString("\n")
  private val mixedText = Vector.tabulate(40)(line => s"line $line 中文 mixed ★ text 中 and more 文文 end").mkString("\n")

  private def configWith(families: List[String]): AppConfig =
    val base = AppConfig.conventional
    base.withFontConfig(base.editorConfig.fontConfig.copy(fallbackFamilies = families))

  final private class PersistingImage:
    private val image  = LaptopFrameBenchmarks.frameImage()
    private val caches = RenderCaches.create()
    private val screen = ScreenIdentity(this)

    def render(state: AppState, damage: Damage): BufferedImage =
      val surface = new Java2DRenderSurface(
        image,
        cellMetrics,
        codeFont,
        _ => (),
        logicalWidthPx = LaptopFrameBenchmarks.LogicalWidthPx,
        logicalHeightPx = LaptopFrameBenchmarks.LogicalHeightPx,
        deviceScaleX = LaptopFrameBenchmarks.DeviceScale,
        deviceScaleY = LaptopFrameBenchmarks.DeviceScale,
        contentPersists = true,
        layerCacheOwnerOverride = Some(screen)
      )
      val _ = RendererFramePlanner.renderFrame(
        state,
        cursorVisible = false,
        surface,
        viewport,
        caches.authoritativeScene.forState(state, viewport, codeFont, textFont),
        codeFont,
        textFont,
        uiFont,
        cellMetrics,
        uiMetrics,
        None,
        Some(FrameOutput(screen, new AtomicReference(None))),
        damage,
        caches = caches
      )
      image

  private def coldFrame(state: AppState): BufferedImage = PersistingImage().render(state, Damage.Everything)

  private def differences(a: BufferedImage, b: BufferedImage): Seq[(Int, Int)] =
    for
      x <- 0 until a.getWidth
      y <- 0 until a.getHeight
      if (a.getRGB(x, y) & 0xffffff) != (b.getRGB(x, y) & 0xffffff)
    yield (x, y)

  private def differing(a: BufferedImage, b: BufferedImage): Int = differences(a, b).size

  private def summary(found: Seq[(Int, Int)], drawn: BufferedImage, cold: BufferedImage): String =
    if found.isEmpty then "no pixels differ"
    else
      val xs = found.map(_._1)
      val ys = found.map(_._2)
      val samples = found.distinctBy(_._2).take(6).map { (x, y) =>
        f"($x,$y) drawn ${drawn.getRGB(x, y) & 0xffffff}%06x cold ${cold.getRGB(x, y) & 0xffffff}%06x"
      }
      s"${found.size} pixels differ, x ${xs.min}-${xs.max}, y ${ys.min}-${ys.max}; ${samples.mkString("; ")}"

  private def typingTransition(text: String, config: AppConfig): (AppState, AppState) =
    val (stateManager, _) = LaptopFrameBenchmarks.stateManagerHolding(text, config, cursorLine = 10, cursorColumn = 8)
    val before            = stateManager.getCurrentState.unsafeRunSync()
    stateManager.applyEvent(InsertChar('x'): Event).unsafeRunSync()
    (before, stateManager.getCurrentState.unsafeRunSync())

  "The fixture" should "use physical fonts, so nothing falls back by itself" in {
    FontLoader.isMonospacedFont(codeFont) shouldBe true
    codeFont.canDisplay('中') shouldBe false
  }

  "A frame over text that needs fallback fonts" should "differ from one drawn without the configured families" in {
    val (_, withFallback)    = typingTransition(mixedText, configWith(List(fallbackFamily)))
    val (_, withoutFallback) = typingTransition(mixedText, configWith(Nil))
    differing(coldFrame(withFallback), coldFrame(withoutFallback)) should be > 0
  }

  private def equalsColdFrameThereAndBack(text: String, families: List[String]): Unit =
    val (before, after) = typingTransition(text, configWith(families))
    val frames          = PersistingImage()
    val _               = frames.render(before, Damage.Everything)

    val there      = frames.render(after, DamageProducer.forTransition(before, after))
    val coldAfter  = coldFrame(after)
    val thereDiffs = differences(there, coldAfter)
    withClue(s"the frame typed into, ${summary(thereDiffs, there, coldAfter)}: ")(thereDiffs shouldBe empty)

    val back       = frames.render(before, DamageProducer.forTransition(after, before))
    val coldBefore = coldFrame(before)
    val backDiffs  = differences(back, coldBefore)
    withClue(s"the frame typed back out of, ${summary(backDiffs, back, coldBefore)}: ")(backDiffs shouldBe empty)

  "A cold frame" should "equal another cold frame of the same state, over text drawn in a configured fallback font" in {
    val (state, _) = typingTransition(mixedText, configWith(List(fallbackFamily)))
    val first      = coldFrame(state)
    val second     = coldFrame(state)
    val found      = differences(first, second)
    withClue(s"two cold frames, ${summary(found, first, second)}: ")(found shouldBe empty)
  }

  it should "equal another cold frame of the same state, over text that needs fallback fonts when none are configured" in {
    val (state, _) = typingTransition(mixedText, configWith(Nil))
    val first      = coldFrame(state)
    val second     = coldFrame(state)
    val found      = differences(first, second)
    withClue(s"two cold frames, ${summary(found, first, second)}: ")(found shouldBe empty)
  }

  "An incremental frame" should "equal a cold one over plain text, there and back" in
    equalsColdFrameThereAndBack(plainText, Nil)

  it should "equal a cold one over text that needs fallback fonts when none are configured" in
    equalsColdFrameThereAndBack(mixedText, Nil)

  it should "equal a cold one over text drawn in a configured fallback font" in
    equalsColdFrameThereAndBack(mixedText, List(fallbackFamily))
