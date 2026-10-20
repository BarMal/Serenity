package com.serenity.ui.renderer

import java.awt.Font
import java.awt.font.FontRenderContext
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.{AtomicLong, AtomicReference}

/** What was read off a font by shaping a few characters of it, kept by the owner that asked: whether its layout advance
  * is the sum of its glyphs', whether its tables substitute glyphs, whether its advances are uniform, how a pair of
  * characters kerns, whether a row may be cut before a character, and whether it is measured glyph by glyph.
  *
  * Every kind of probe shares one bounded memo. Two generations stand in for an eviction order, as in
  * [[ShapedTextCache]]: a hit in the older one moves the answer to the newer, and when the newer fills the older is
  * dropped, so the memo holds at most twice `capacityPerGeneration` answers however many fonts an owner meets. An
  * answer that was dropped is read off the font again.
  */
final class FontProbes(capacityPerGeneration: Int = FontProbes.DefaultCapacity):
  import FontProbes.*

  private val generations = AtomicReference(Generations(newMap(), newMap()))
  private val probes      = AtomicLong(0)

  /** How many probes ran, as opposed to being answered from what an earlier one found. */
  def probesRun: Long = probes.get

  private def lookup(key: Key): Option[AnyRef] =
    val held = generations.get()
    Option(held.current.get(key)).orElse(Option(held.previous.get(key)).map(remember(key, _)))

  private def flag(key: Key)(answer: => Boolean): Boolean =
    lookup(key) match
      case Some(found: java.lang.Boolean) => found
      case _ =>
        val _        = probes.incrementAndGet()
        val measured = answer
        val _        = remember(key, measured: java.lang.Boolean)
        measured

  private def kerning(key: Key)(answer: => Float): Float =
    lookup(key) match
      case Some(found: java.lang.Float) => found
      case _ =>
        val _        = probes.incrementAndGet()
        val measured = answer
        val _        = remember(key, measured: java.lang.Float)
        measured

  private def remember(key: Key, answer: AnyRef): AnyRef =
    val held = generations.get()
    if held.current.size < capacityPerGeneration then
      val _ = held.current.put(key, answer)
    else
      val next = Generations(newMap(), held.current)
      val _    = next.current.put(key, answer)
      val _    = generations.compareAndSet(held, next)
    answer

  def layoutAgreement(key: Probe)(read: Probe => Boolean): Boolean =
    flag(Key(Kind.LayoutAgreement, key.font, key.frc, NoChar, NoChar))(read(key))

  def substitution(key: Probe)(read: Probe => Boolean): Boolean =
    flag(Key(Kind.Substitution, key.font, key.frc, NoChar, NoChar))(read(key))

  def uniformAdvance(key: Probe)(read: Probe => Boolean): Boolean =
    flag(Key(Kind.UniformAdvance, key.font, key.frc, NoChar, NoChar))(read(key))

  def pairKerning(key: PairProbe)(read: PairProbe => Float): Float =
    kerning(Key(Kind.PairKerning, key.font, key.frc, key.first, key.second))(read(key))

  def barrier(key: BarrierProbe)(read: BarrierProbe => Boolean): Boolean =
    flag(Key(Kind.Barrier, key.font, key.frc, key.next, NoChar))(read(key))

  def measuredLayout(key: Probe)(read: Probe => Boolean): Boolean =
    flag(Key(Kind.MeasuredLayout, key.font, key.frc, NoChar, NoChar))(read(key))

object FontProbes:

  val DefaultCapacity: Int = 4096

  final case class Probe(font: Font, frc: FontRenderContext)

  final case class PairProbe(font: Font, frc: FontRenderContext, first: Char, second: Char)

  final case class BarrierProbe(font: Font, frc: FontRenderContext, next: Char)

  private enum Kind:
    case LayoutAgreement, Substitution, UniformAdvance, PairKerning, Barrier, MeasuredLayout

  private val NoChar = '\u0000'

  final private case class Key(kind: Kind, font: Font, frc: FontRenderContext, first: Char, second: Char)

  private type Answers = ConcurrentHashMap[Key, AnyRef]

  final private case class Generations(current: Answers, previous: Answers)

  private def newMap(): Answers = new ConcurrentHashMap[Key, AnyRef]()
