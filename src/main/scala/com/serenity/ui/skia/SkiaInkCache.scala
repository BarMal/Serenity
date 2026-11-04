package com.serenity.ui.skia

import java.util.concurrent.ConcurrentHashMap

import org.jetbrains.skia.Shader

/** The colours text is drawn in, as shaders. Skia draws anti-aliased text in a plain-colour paint with a mask blit that
  * is several times slower than the one a shader gets on the CPU rasteriser (1.5 ms for 20 lines of 100 characters at
  * 2x against 5.6 ms), and a shader is a native object, so each colour has one that every frame reuses. Few colours are
  * in use at once; past `limit` they are all dropped, so a long session of ever-different colours holds no more.
  */
final class SkiaInkCache(limit: Int = SkiaInkCache.DefaultLimit):

  private val shaders = new ConcurrentHashMap[Int, Shader]()

  /** How many colours have a shader now. */
  def size: Int = shaders.size

  /** The shader that paints `argb`, alpha included. */
  def shaderOf(argb: Int): Shader =
    if shaders.size >= limit && !shaders.containsKey(argb) then clear()
    shaders.computeIfAbsent(argb, colour => Shader.Companion.makeColor(colour))

  private def clear(): Unit =
    val held = shaders.values().toArray(new Array[Shader](0))
    shaders.clear()
    held.foreach(_.close())

object SkiaInkCache:
  val DefaultLimit: Int = 64
