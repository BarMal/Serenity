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

  it should "default eco to false" in {
    LaunchOptions.parse(List("notes.md")).eco shouldBe false
  }

  it should "recognise a bare --eco flag" in {
    LaunchOptions.parse(List("--eco")).eco shouldBe true
  }

  it should "recognise --eco alongside an open path, regardless of order" in {
    LaunchOptions.parse(List("--eco", "notes.md")) shouldBe LaunchOptions(
      openPath = Some(Path.of("notes.md")),
      eco = true
    )
    LaunchOptions.parse(List("notes.md", "--eco")) shouldBe LaunchOptions(
      openPath = Some(Path.of("notes.md")),
      eco = true
    )
  }

  it should "recognise --eco alongside --open" in {
    LaunchOptions.parse(List("--eco", "--open", "notes.md")) shouldBe LaunchOptions(
      openPath = Some(Path.of("notes.md")),
      eco = true
    )
  }
