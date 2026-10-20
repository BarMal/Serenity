package com.serenity.ui.renderer

import java.awt.Font
import java.awt.font.FontRenderContext
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicLong

/** What was read off a font by shaping a few characters of it, kept by the owner that asked: whether its layout advance
  * is the sum of its glyphs', whether its tables substitute glyphs, whether its advances are uniform, how a pair of
  * characters kerns, whether a row may be cut before a character, and whether it is measured glyph by glyph.
  *
  * A probe reads the font, not the text, so each is answered once per font and render context for the life of the owner
  * (a [[ShapedTextCache]] carries one, so whatever shapes text for an owner probes for it).
  */
final class FontProbes:
  import FontProbes.*

  private val probes          = AtomicLong(0)
  private val agreements      = ConcurrentHashMap[Probe, java.lang.Boolean]()
  private val substitutions   = ConcurrentHashMap[Probe, java.lang.Boolean]()
  private val uniformAdvances = ConcurrentHashMap[Probe, java.lang.Boolean]()
  private val pairKernings    = ConcurrentHashMap[PairProbe, java.lang.Float]()
  private val barriers        = ConcurrentHashMap[BarrierProbe, java.lang.Boolean]()
  private val measuredLayouts = ConcurrentHashMap[Probe, java.lang.Boolean]()

  /** How many probes ran, as opposed to being answered from what an earlier one found. */
  def probesRun: Long = probes.get

  private def counted[A](probe: => A): A =
    val _ = probes.incrementAndGet()
    probe

  def layoutAgreement(key: Probe)(read: Probe => Boolean): Boolean =
    agreements.computeIfAbsent(key, probe => counted(read(probe)): java.lang.Boolean)

  def substitution(key: Probe)(read: Probe => Boolean): Boolean =
    substitutions.computeIfAbsent(key, probe => counted(read(probe)): java.lang.Boolean)

  def uniformAdvance(key: Probe)(read: Probe => Boolean): Boolean =
    uniformAdvances.computeIfAbsent(key, probe => counted(read(probe)): java.lang.Boolean)

  def pairKerning(key: PairProbe)(read: PairProbe => Float): Float =
    pairKernings.computeIfAbsent(key, probe => counted(read(probe)): java.lang.Float)

  def barrier(key: BarrierProbe)(read: BarrierProbe => Boolean): Boolean =
    barriers.computeIfAbsent(key, probe => counted(read(probe)): java.lang.Boolean)

  def measuredLayout(key: Probe)(read: Probe => Boolean): Boolean =
    measuredLayouts.computeIfAbsent(key, probe => counted(read(probe)): java.lang.Boolean)

object FontProbes:

  final case class Probe(font: Font, frc: FontRenderContext)

  final case class PairProbe(font: Font, frc: FontRenderContext, first: Char, second: Char)

  final case class BarrierProbe(font: Font, frc: FontRenderContext, next: Char)
