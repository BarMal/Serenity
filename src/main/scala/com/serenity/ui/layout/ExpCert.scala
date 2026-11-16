package com.serenity.ui.layout

import java.util.concurrent.ConcurrentHashMap

import com.serenity.ui.renderer.FontSpec

/** SCRATCH, never merged: the flags of the #2660 D2 certificate-cost experiments, read from SERENITY_EXP (a comma list
  * of a, b, c, d).
  */
private[serenity] object ExpCert:

  val flags: Set[String] = sys.env.getOrElse("SERENITY_EXP", "").split(",").map(_.trim).filter(_.nonEmpty).toSet

  def on(flag: String): Boolean = flags.contains(flag)

  final private case class PerMeasurer(font: FontSpec, measurer: AnyRef)

  private val shared = new ConcurrentHashMap[AnyRef, Option[NominalAdvances]]()

  /** (a) one certificate per font and size for the process; (b) one per font and size for each measurer. */
  def cached(font: FontSpec, measurer: AnyRef)(build: => Option[NominalAdvances]): Option[NominalAdvances] =
    if on("a") then shared.computeIfAbsent(font, _ => build)
    else if on("b") then shared.computeIfAbsent(PerMeasurer(font, measurer), _ => build)
    else build

  private val offLock = new ThreadLocal[java.lang.Boolean]()

  /** (c) the certificate shapes on a Shaper and lock of its own. */
  def offShaperLock: Boolean = Option(offLock.get).exists(_.booleanValue)

  def certifying[A](body: => A): A =
    if on("c") then
      offLock.set(java.lang.Boolean.TRUE)
      try body
      finally offLock.remove()
    else body
