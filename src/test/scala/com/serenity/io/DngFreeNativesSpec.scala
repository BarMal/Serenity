package com.serenity.io

import java.nio.file.{Files, Paths}

import scala.io.Source

import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The Skiko natives Serenity ships are built without the Adobe DNG SDK (#2028), whose licence conflicts with the GPL,
  * so nothing about it may remain in the notices, and the runtime classpath may carry only the project's own build of
  * the natives, never JetBrains' stock jar that contains it.
  */
class DngFreeNativesSpec extends AnyFlatSpec with Matchers:

  private def runtimeModules: List[String] =
    val source = Source.fromInputStream(getClass.getResourceAsStream("/licences/runtime-modules.txt"), "UTF-8")
    try source.getLines().toList.filter(_.nonEmpty)
    finally source.close()

  "The runtime classpath" should "carry the project's own Skiko runtime natives" in {
    runtimeModules.filter(_.startsWith("io.github.barmal:skiko-awt-runtime-")) should have size 1
  }

  it should "not carry JetBrains' stock Skiko runtime natives, which contain the DNG SDK" in {
    runtimeModules.filter(_.startsWith("org.jetbrains.skiko:skiko-awt-runtime-")) shouldBe empty
  }

  it should "still take the Skiko API classes from Maven Central" in {
    runtimeModules should contain("org.jetbrains.skiko:skiko-awt")
  }

  "The shipped notices" should "carry neither the Adobe DNG SDK's licence nor an entry for it or for PIEX" in {
    val notices = LicenceNotices.notices.unsafeRunSync()

    notices should not include "DNG SDK License Agreement"
    notices should not include "| Adobe DNG SDK |"
    notices should not include "| PIEX |"
    notices should not include "dng_sdk"
  }

  they should "list the project's own Skiko natives with their licence" in {
    val notices = LicenceNotices.notices.unsafeRunSync()

    notices should include("| io.github.barmal:skiko-awt-runtime-")
    notices should not include "| org.jetbrains.skiko:skiko-awt-runtime-"
  }

  "The third-party texts" should "hold no Adobe DNG SDK licence" in {
    Files.exists(Paths.get("third-party", "texts", "Adobe-DNG-SDK.txt")) shouldBe false
  }
