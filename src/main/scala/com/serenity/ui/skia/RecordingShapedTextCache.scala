package com.serenity.ui.skia

import scala.jdk.CollectionConverters.*

import org.jetbrains.skia.Font

/** A shaped-text cache that shapes nothing: every ask is noted and answered with a stretch that draws nothing. */
final private[skia] class RecordingShapedTextCache extends SkiaShapedTextCache:

  private val asked = new java.util.LinkedHashSet[(String, Font)]()

  override def stretchOf(text: String, font: Font): SkiaShapedStretch =
    val _ = asked.synchronized(asked.add(text -> font))
    UnshapedStretch

  /** What was asked, each once, in the order it was first asked. */
  def keys: Vector[(String, Font)] = asked.synchronized(Vector.from(asked.iterator.asScala))
