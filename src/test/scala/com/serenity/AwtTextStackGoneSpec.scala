package com.serenity

import java.nio.file.{Files, Path, Paths}

import scala.jdk.CollectionConverters.*
import scala.util.Using

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** What #2660 item 2, step D3, removes from main: the AWT text stack that Skia's measurer and caches replaced. Each
  * test is one change of that step and names what must no longer be there.
  */
final class AwtTextStackGoneSpec extends AnyFlatSpec with Matchers:

  private val Main = Paths.get("src", "main", "scala")

  private def mainFiles: List[Path] =
    Using.resource(Files.walk(Main))(stream =>
      stream.iterator().asScala.filter(path => path.toString.endsWith(".scala")).toList.sorted
    )

  private def filesMentioning(pattern: String): List[String] =
    val regex = pattern.r
    mainFiles
      .filter(path => regex.findFirstIn(Files.readString(path)).isDefined)
      .map(path => Main.relativize(path).toString)

  "Main" should "hold no AWT shaping stack: no shaped-text cache, shaped stretch, glyph-advance table or font probes" in {
    filesMentioning("""\b(ShapedTextCache|ShapedStretch|GlyphAdvance|FontProbes)\b""") shouldBe Nil
  }
