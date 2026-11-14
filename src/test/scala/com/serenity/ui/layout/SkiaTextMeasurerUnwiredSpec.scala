package com.serenity.ui.layout

import java.nio.file.{Files, Paths}

import scala.jdk.CollectionConverters.*
import scala.util.Using

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Until layout is switched to the `TextMeasurer` seam (#2660 item 2, step D), nothing in the app measures with
  * [[com.serenity.ui.skia.SkiaTextMeasurer]]: it is only compared with the checked-in AWT measurements. This fails the
  * pull request that wires it, so that wiring is a decision and not a side effect.
  */
class SkiaTextMeasurerUnwiredSpec extends AnyFlatSpec with Matchers:

  "No main source but SkiaTextMeasurer itself" should "name SkiaTextMeasurer" in {
    val root = Paths.get("src", "main")
    val users = Using.resource(Files.walk(root)) { stream =>
      stream
        .iterator()
        .asScala
        .filter(path => Files.isRegularFile(path) && path.toString.endsWith(".scala"))
        .filterNot(_.getFileName.toString == "SkiaTextMeasurer.scala")
        .filter(path => Files.readString(path).contains("SkiaTextMeasurer"))
        .map(path => root.relativize(path).toString)
        .toList
    }

    users shouldBe Nil
  }
