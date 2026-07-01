package com.serenity

import java.nio.file.Path

import com.serenity.app.LaunchOptions
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LaunchOptionsSpec extends AnyFlatSpec with Matchers:

  "LaunchOptions.parse" should "accept an explicit open path" in {
    LaunchOptions.parse(List("--open", "notes.md")).openPath shouldBe Some(Path.of("notes.md"))
  }

  it should "accept a file alias" in {
    LaunchOptions.parse(List("--file", "notes.md")).openPath shouldBe Some(Path.of("notes.md"))
  }

  it should "accept a bare launch path" in {
    LaunchOptions.parse(List("notes.md")).openPath shouldBe Some(Path.of("notes.md"))
  }

  it should "ignore unsupported options" in {
    LaunchOptions.parse(List("--unknown", "notes.md")).openPath shouldBe None
  }
