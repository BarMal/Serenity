package com.serenity.ui.renderer

import java.awt.Color

import com.serenity.MockRenderSurface
import com.serenity.animation.{AnimatedCell, AnimationState, CharacterKey, EasingCurve, Tween}
import com.serenity.state.models.{TextCaretStop, TextVisualLine}
import com.serenity.ui.theme.{StyledText, TextStyle, Theme}
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** `renderMeasuredLineWithAnimation` must issue exactly the draw calls -- text, colours, style, pixel extent -- that
  * the implementation it replaced did. [[OriginalMeasuredLine]] is that implementation, kept verbatim as the oracle.
  */
class MeasuredLineRunsEquivalenceSpec extends AnyPropSpec with ScalaCheckPropertyChecks with Matchers:

  private val theme   = Theme.light
  private val palette = Vector(Color.RED, Color.BLUE, theme.foreground, theme.background)
  private val styles  = Vector(TextStyle.normal, TextStyle(isBold = true), TextStyle(isItalic = true))

  private val genCluster: Gen[String] =
    Gen.frequency(
      8 -> Gen.alphaNumChar.map(_.toString),
      2 -> Gen.const(" "),
      1 -> Gen.const("é"),
      1 -> Gen.const("😀"),
      1 -> Gen.const("👩‍💻"),
      1 -> Gen.const("\t")
    )

  private val genText: Gen[String] = Gen.choose(1, 40).flatMap(n => Gen.listOfN(n, genCluster)).map(_.mkString)

  /** Segments covering `text` in random pieces; sometimes stopping short, so lookups past their end fall back. */
  private def genSegments(text: String): Gen[List[StyledText]] =
    def pieces(from: Int): Gen[List[StyledText]] =
      if from >= text.length then Gen.const(Nil)
      else
        for
          length <- Gen.choose(0, text.length - from)
          fg     <- Gen.oneOf(palette)
          bg     <- Gen.oneOf(palette)
          style  <- Gen.oneOf(styles)
          rest   <- if length == 0 then Gen.const(Nil) else pieces(from + length)
        yield StyledText(text.substring(from, from + length), style, fg, bg) :: rest
    Gen.frequency(4 -> pieces(0), 1 -> pieces(0).map(_.dropRight(1)), 1 -> Gen.const(Nil))

  /** Caret stops at cluster boundaries, rising left to right; sometimes one is dropped or the line is shifted. */
  private def genLine(text: String): Gen[TextVisualLine] =
    val boundaries = CharacterRenderer.computeGraphemeSpans(text).map(_.startLocalIndex) :+ text.length
    for
      startColumn <- Gen.choose(0, 3)
      widths      <- Gen.listOfN(boundaries.length, Gen.choose(0.0f, 9.5f))
      dropAt      <- Gen.option(Gen.choose(0, boundaries.length - 1))
      reversed    <- Gen.frequency(9 -> false, 1 -> true)
      xs       = widths.scanLeft(0.0f)(_ + _).take(boundaries.length).toVector
      placed   = if reversed then xs.reverse else xs
      allStops = boundaries.zip(placed).map((column, x) => TextCaretStop(startColumn + column, x))
      stops    = dropAt.fold(allStops)(at => allStops.patch(at, Nil, 1))
    yield TextVisualLine(2, startColumn, startColumn + text.length, text, placed.lastOption.getOrElse(0.0f), stops)

  private def genAnimations(line: TextVisualLine): Gen[AnimationState] =
    Gen
      .listOf(
        for
          column <- Gen.choose(line.startColumn, line.endColumn)
          fg     <- Gen.option(Gen.oneOf(palette))
          bg     <- Gen.option(Gen.oneOf(palette))
        yield CharacterKey(column, line.bufferLine) -> AnimatedCell(
          Some('x'),
          fg.map(c => Tween(c, c, EasingCurve.Linear, steps = 1)),
          bg.map(c => Tween(c, c, EasingCurve.Linear, steps = 1))
        )
      )
      .map(cells => AnimationState(cells.toMap))

  /** Each surface's call records are instances of its own inner class, so they are compared by their fields. */
  private def drawn(surface: MockRenderSurface): List[List[Any]] = surface.drawRunPxCalls.map(_.productIterator.toList)

  private val genCase =
    for
      text       <- genText
      line       <- genLine(text)
      segments   <- Gen.option(genSegments(text))
      animations <- Gen.frequency(3 -> Gen.const(AnimationState.empty), 2 -> genAnimations(line))
      clipRight  <- Gen.option(Gen.choose(0.0f, 200.0f))
      xOrigin    <- Gen.choose(0.0f, 30.0f)
    yield (line, segments, animations, clipRight, xOrigin)

  property("draws the same runs as the original implementation") {
    forAll(genCase, minSuccessful(500)) {
      case (line, segments, animations, clipRight, xOrigin) =>
        val actual   = new MockRenderSurface(400, 24)
        val expected = new MockRenderSurface(400, 24)
        CharacterRenderer.renderMeasuredLineWithAnimation(
          actual,
          xOrigin,
          16,
          18,
          14,
          line,
          theme,
          animations,
          styledSegments = segments,
          clipRightXPx = clipRight
        )
        OriginalMeasuredLine.render(expected, xOrigin, 16, 18, 14, line, theme, animations, segments, clipRight)
        drawn(actual) shouldBe drawn(expected)
    }
  }

/** The measured-line renderer as it stood before runs were built by `MeasuredLineRuns`, unchanged apart from taking its
  * optional inputs positionally.
  */
private object OriginalMeasuredLine:

  final private case class GraphemeBounds(startLocalIndex: Int, endLocalIndex: Int, startXPx: Float, endXPx: Float)

  final private case class MeasuredGrapheme(
      text: String,
      foreground: Color,
      background: Color,
      style: TextStyle,
      startLocalIndex: Int,
      endLocalIndex: Int
  )

  def render(
    surface: RenderSurface,
    xOriginPx: Float,
    yPx: Int,
    lineHeightPx: Int,
    ascentPx: Int,
    visualLine: TextVisualLine,
    theme: Theme,
    animations: AnimationState,
    styledSegments: Option[List[StyledText]],
    clipRightXPx: Option[Float]
  ): Unit =
    val text = visualLine.text
    if text.nonEmpty then
      val stops = visualLine.caretStops
      val styledSegments0 =
        styledSegments.getOrElse(List(StyledText(text, TextStyle.normal, theme.foreground, theme.background)))

      final case class MeasuredRun(
          startLocalIndex: Int,
          foreground: Color,
          background: Color,
          style: TextStyle,
          text: StringBuilder,
          endLocalIndex: Int
      )

      val spans = CharacterRenderer.computeGraphemeSpans(text)

      val graphemeBounds =
        @annotation.tailrec
        def collect(spanIndex: Int, stopIndex: Int, acc: List[GraphemeBounds]): List[GraphemeBounds] =
          if spanIndex >= spans.length || stopIndex + 1 >= stops.length then acc.reverse
          else
            val span      = spans(spanIndex)
            val startStop = stops.lift(stopIndex).filter(_.column == visualLine.startColumn + span.startLocalIndex)
            val endStop   = stops.lift(stopIndex + 1).filter(_.column == visualLine.startColumn + span.endLocalIndex)
            (startStop, endStop) match
              case (Some(start), Some(end)) =>
                collect(
                  spanIndex + 1,
                  stopIndex + 1,
                  GraphemeBounds(span.startLocalIndex, span.endLocalIndex, start.xPx, end.xPx) :: acc
                )
              case _ => collect(spanIndex, stopIndex + 1, acc)

        collect(0, 0, Nil)

      def visualExtentsForRange(startLocalIndex: Int, endLocalIndex: Int): (Float, Float) =
        graphemeBounds
          .foldLeft(Option.empty[(Float, Float)]) {
            case (acc, GraphemeBounds(clusterStart, clusterEnd, startXPx, endXPx))
                if clusterStart < endLocalIndex && clusterEnd > startLocalIndex =>
              acc match
                case Some((minXPx, maxXPx)) => Some((minXPx.min(startXPx), maxXPx.max(endXPx)))
                case None                   => Some((startXPx, endXPx))
            case (acc, _) => acc
          }
          .getOrElse((0.0f, 0.0f))

      def drawRun(run: MeasuredRun): Unit =
        val (minXPx, maxXPx) = visualExtentsForRange(run.startLocalIndex, run.endLocalIndex)
        val startXPx         = xOriginPx + minXPx
        val endXPx           = xOriginPx + maxXPx
        val clippedEndXPx    = clipRightXPx.fold(endXPx)(_.min(endXPx))
        val widthPx          = clippedEndXPx - startXPx
        if widthPx > 0.0f then
          surface.setForegroundColor(run.foreground)
          surface.setBackgroundColor(run.background)
          surface.enableStyle(run.style)
          try surface.text.drawRunPx(startXPx, yPx, widthPx, lineHeightPx, ascentPx, run.text.toString)
          finally surface.disableStyle(run.style)

      val chars = styledSegments0
        .flatMap(segment =>
          segment.content.map(char => (char, segment.foregroundColor, segment.backgroundColor, segment.style))
        )
        .toVector
      val hasAnimations = animations.animations.nonEmpty

      val graphemeChars: Vector[MeasuredGrapheme] =
        spans.map { span =>
          val segmentStyle      = chars.lift(span.startLocalIndex).map(_._4).getOrElse(TextStyle.normal)
          val segmentForeground = chars.lift(span.startLocalIndex).map(_._2).getOrElse(theme.foreground)
          val segmentBackground = chars.lift(span.startLocalIndex).map(_._3).getOrElse(theme.background)
          MeasuredGrapheme(
            text.substring(span.startLocalIndex, span.endLocalIndex),
            segmentForeground,
            segmentBackground,
            segmentStyle,
            span.startLocalIndex,
            span.endLocalIndex
          )
        }

      val (runs, currentRun, _) = graphemeChars.foldLeft((List.empty[MeasuredRun], Option.empty[MeasuredRun], 0)) {
        case (
              (completed, current, localIndex),
              MeasuredGrapheme(grapheme, segmentForeground, segmentBackground, style, graphemeStart, graphemeEnd)
            ) =>
          val bufferColumn = visualLine.startColumn + graphemeStart
          val cell         = if hasAnimations then animations.getCell(bufferColumn, visualLine.bufferLine) else None
          val foreground   = cell.flatMap(_.currentForeground).getOrElse(segmentForeground)
          val background   = cell.flatMap(_.currentBackground).getOrElse(segmentBackground)

          current match
            case None =>
              val run = MeasuredRun(localIndex, foreground, background, style, StringBuilder(grapheme), graphemeEnd)
              (completed, Some(run), graphemeEnd)
            case Some(run) if foreground == run.foreground && background == run.background && style == run.style =>
              run.text.append(grapheme)
              (completed, Some(run.copy(endLocalIndex = graphemeEnd)), graphemeEnd)
            case Some(run) =>
              val nextRun =
                MeasuredRun(graphemeStart, foreground, background, style, StringBuilder(grapheme), graphemeEnd)
              (run :: completed, Some(nextRun), graphemeEnd)
      }

      (currentRun.toList ::: runs).reverse.foreach(drawRun)
