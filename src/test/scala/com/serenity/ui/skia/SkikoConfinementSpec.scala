package com.serenity.ui.skia

import java.nio.file.{Files, Path, Paths}

import scala.jdk.CollectionConverters.*
import scala.util.Using

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The static half of keeping Skiko lazy: only `com.serenity.ui.skia` may name Skiko, Skia or Kotlin. Everything else
  * reaches Skia through [[com.serenity.ui.renderer.FrameSurfaceFactory]], which is built on demand.
  */
class SkikoConfinementSpec extends AnyFlatSpec with Matchers:

  private val mainRoot   = Paths.get("src", "main")
  private val skiaFolder = "com/serenity/ui/skia/"

  private val Forbidden = List("org.jetbrains.skia", "org.jetbrains.skiko", "import kotlin.")

  private def mainSources: List[Path] =
    Using.resource(Files.walk(mainRoot)) { stream =>
      stream
        .iterator()
        .asScala
        .filter(path => Files.isRegularFile(path) && path.toString.endsWith(".scala"))
        .toList
    }

  private def referencesSkia(line: String): Boolean = Forbidden.exists(line.contains)

  "Skiko references" should "stay inside com.serenity.ui.skia" in {
    val offenders = mainSources
      .filterNot(path => mainRoot.relativize(path).toString.replace('\\', '/').contains(skiaFolder))
      .filter(path => Files.readAllLines(path).asScala.exists(referencesSkia))
      .map(path => mainRoot.relativize(path).toString)
    offenders shouldBe Nil
  }

  it should "be noticed when a file outside the folder imports one" in {
    referencesSkia("import org.jetbrains.skiko.Library") shouldBe true
    referencesSkia("val s = org.jetbrains.skia.Surface") shouldBe true
    referencesSkia("import cats.effect.IO") shouldBe false
  }
