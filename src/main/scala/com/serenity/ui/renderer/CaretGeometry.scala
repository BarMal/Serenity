package com.serenity.ui.renderer

import java.awt.Rectangle

import com.serenity.state.models.CaretStyle

/** The shapes a styled caret is drawn as, shared by every backend so they cannot disagree. `body` is the rect the
  * layout placed the caret in; nothing here moves it. A slant pivots on the base, a bold caret grows to the right, and
  * the underline and strikethrough rules are centred on the caret.
  */
object CaretGeometry:

  final case class Polygon(points: Vector[(Double, Double)])

  private val SlantOfHeight  = 0.2
  private val RuleOfHeight   = 0.5
  private val RuleOfCaretMin = 4

  def polygons(body: Rectangle, style: CaretStyle): List[Polygon] =
    val (x, y, width, height) = (body.x, body.y, body.width, body.height)
    val thickness             = math.max(1, width / 2)
    val ruleLength            = math.max(RuleOfCaretMin * width, math.round(height * RuleOfHeight).toInt)
    val ruleLeft              = math.round(x + width / 2.0 - ruleLength / 2.0).toInt
    val ruleRight             = ruleLeft + ruleLength
    val underline = Option.when(style.underline)(rect(ruleLeft, y + height - thickness, ruleRight, y + height))
    val strike =
      Option.when(style.strikethrough) {
        val top = y + (height - thickness) / 2
        rect(ruleLeft, top, ruleRight, top + thickness)
      }
    bodyPolygon(body, style) :: underline.toList ::: strike.toList

  /** The caret's own shape, slanted and widened as styled, moved by [[TextShadow]]'s offset: what a shadowed caret draws
    * first, in the shadow's colour. The rules are not shadowed, as an underline is not in the text.
    */
  def shadowPolygons(body: Rectangle, style: CaretStyle): List[Polygon] =
    val offset = TextShadow.offsetOf(1.0)
    Option
      .when(style.shadow)(bodyPolygon(body, style).copy(points = bodyPolygon(body, style).points.map((x, y) => (x + offset, y + offset))))
      .toList

  /** The smallest rect covering every shape of `style`, which a repaint of the caret has to cover. */
  def bounds(body: Rectangle, style: CaretStyle): Rectangle =
    val points = (polygons(body, style) ++ shadowPolygons(body, style)).flatMap(_.points)
    val xs     = points.map(_._1)
    val ys     = points.map(_._2)
    val minX   = math.floor(xs.foldLeft(Double.PositiveInfinity)(math.min)).toInt
    val maxX   = math.ceil(xs.foldLeft(Double.NegativeInfinity)(math.max)).toInt
    val minY   = math.floor(ys.foldLeft(Double.PositiveInfinity)(math.min)).toInt
    val maxY   = math.ceil(ys.foldLeft(Double.NegativeInfinity)(math.max)).toInt
    new Rectangle(minX, minY, maxX - minX, maxY - minY)

  private def bodyPolygon(body: Rectangle, style: CaretStyle): Polygon =
    val (x, y, width, height) = (body.x, body.y, body.width, body.height)
    val bodyRight             = x + (if style.bold then width * 2 else width)
    val slant                 = if style.italic then math.round(height * SlantOfHeight).toInt else 0
    if slant == 0 then rect(x, y, bodyRight, y + height)
    else Polygon(Vector((x, y + height), (bodyRight, y + height), (bodyRight + slant, y), (x + slant, y)).map(widen))

  private def rect(left: Int, top: Int, right: Int, bottom: Int): Polygon =
    Polygon(Vector((left, bottom), (right, bottom), (right, top), (left, top)).map(widen))

  private def widen(point: (Int, Int)): (Double, Double) = (point._1.toDouble, point._2.toDouble)
