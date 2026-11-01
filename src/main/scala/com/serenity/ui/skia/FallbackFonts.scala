package com.serenity.ui.skia

import java.lang.invoke.{MethodHandles, MethodType}
import java.util.concurrent.ConcurrentHashMap

import scala.util.Try

import org.jetbrains.skia.{Font, FontMgr, Typeface}

/** Which installed font stands in for a character a base font lacks, as Skiko's `TextLine` would pick it: the system
  * font manager's `matchFamilyStyleCharacter` with the base font's style and no family, so the manager's own default
  * family decides (asking for the base font's family picks a different font, e.g. FreeSerif for DejaVu Serif).
  *
  * Each answer is remembered for the life of this owner, so a font installed after a character was first met is not
  * seen until the owner is made again; `TextLine` would see it at once.
  */
final class FallbackFonts private (matcher: Option[FallbackFonts.Matcher]):
  import FallbackFonts.*

  private val chosen = new ConcurrentHashMap[(Int, Int), Option[Typeface]]()

  /** `None` when the character cannot be matched, either because there is no match call here or no font has it. */
  def typefaceFor(base: Typeface, character: Int): Option[Typeface] =
    matcher.flatMap { find =>
      val key = (base.getUniqueId, character)
      Option(chosen.get(key)).getOrElse {
        if chosen.size >= MaxRemembered then chosen.clear()
        val answer = find(base, character)
        val _      = chosen.put(key, answer)
        answer
      }
    }

  /** The font that draws `character` in place of `base`: the matched typeface with the base's size and rendering. */
  def fontFor(base: Font, character: Int): Option[Font] =
    typefaceFor(base.getTypeface, character).map { typeface =>
      val font = new Font(typeface, base.getSize)
      font.setSubpixel(base.isSubpixel)
      font.setLinearMetrics(base.isLinearMetrics)
      font.setHinting(base.getHinting)
      font.setEdging(base.getEdging)
      font.setEmboldened(base.isEmboldened)
      font.setScaleX(base.getScaleX)
      font.setSkewX(base.getSkewX)
      font
    }

object FallbackFonts:

  type Matcher = (Typeface, Int) => Option[Typeface]

  private val MaxRemembered = 4096

  /** With no match call, every character with a missing glyph is left to `TextLine`. */
  def unavailable: FallbackFonts = new FallbackFonts(None)

  def matching(find: Matcher): FallbackFonts = new FallbackFonts(Some(find))

  /** The system's font manager, with a memory of its own: each owner of a shaping cache makes one. Its style parameter
    * is an inline class, so the JVM names carry a hash that Scala cannot write; they are looked up by name, and if a
    * Skiko release changes them there is no match call and `TextLine` keeps shaping these characters, which draws the
    * same pixels.
    */
  def system: FallbackFonts = new FallbackFonts(SystemMatcher)

  /** The request names no family. */
  @SuppressWarnings(Array("org.wartremover.warts.Null"))
  private val noFamily: String = null

  private val SystemMatcher: Option[Matcher] =
    Try {
      val lookup  = MethodHandles.publicLookup()
      val styleOf = lookup.findVirtual(classOf[Typeface], "getFontStyle-Owqf3f4", MethodType.methodType(classOf[Int]))
      val matchCharacter = lookup.findVirtual(
        classOf[FontMgr],
        "matchFamilyStyleCharacter-QWSbI2s",
        MethodType.methodType(classOf[Typeface], classOf[String], classOf[Int], classOf[Array[String]], classOf[Int])
      )
      val manager = FontMgr.Companion.getDefault()
      val find: Matcher = (base, character) =>
        styleOf.invoke(base) match
          case style: java.lang.Integer =>
            val found: Object = matchCharacter.invoke(manager, noFamily, style.intValue, Array.empty[String], character)
            Option(found).collect { case typeface: Typeface => typeface }
          case _ => None
      find
    }.toOption
