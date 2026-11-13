package com.serenity.app

import java.nio.file.Files

import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TrialDocumentSpec extends AnyFlatSpec with Matchers:

  "TrialDocument" should "give a long document of lines that all differ, so every page looks different once scrolled" in
    TrialDocument.resource
      .use { path =>
        cats.effect.IO {
          val lines = Files.readAllLines(path).toArray.toList.map(String.valueOf)
          lines.size should be >= 3000
          lines.distinct.size shouldBe lines.size
          lines.forall(_.nonEmpty) shouldBe true
        }
      }
      .unsafeRunSync()

  it should "remove the file when the trial is over" in {
    val path = TrialDocument.resource.use(path => cats.effect.IO.pure(path)).unsafeRunSync()
    Files.exists(path) shouldBe false
  }
