package com.serenity

import org.jetbrains.skia.FontMgr
import org.scalatest.flatspec.AnyFlatSpec

class SkiaFontDebugSpec extends AnyFlatSpec:

  "The system font manager" should "report what it holds for the logical families" in {
    val manager = FontMgr.Companion.getDefault()
    val names   = (0 until manager.getFamiliesCount).map(index => manager.getFamilyName(index))
    println(s"DEBUG families=${names.size} first=${names.take(5).mkString(",")}")
    List("Serif", "serif", "SansSerif", "sans-serif", "Monospaced", "monospace", "Times New Roman").foreach { family =>
      println(s"DEBUG matchFamily($family) = ${Option(manager.matchFamily(family)).map(_.count())}")
    }
    println(s"DEBUG awt Serif family = ${new java.awt.Font(java.awt.Font.SERIF, java.awt.Font.PLAIN, 14).getFamily}")
    println(s"DEBUG awt Serif font name = ${new java.awt.Font(java.awt.Font.SERIF, java.awt.Font.PLAIN, 14).getFontName}")
  }
