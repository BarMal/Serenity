package com.serenity.text

import scala.annotation.tailrec

import com.serenity.text.TextEditing.{CharacterSource, StringCharacterSource}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Motion by the parts of an identifier (#1949), the commands behind Ctrl+Alt+Left and Ctrl+Alt+Right. */
class TextEditingSubWordSpec extends AnyFlatSpec with Matchers:

  private def forwardStops(text: String): Vector[Int] =
    @tailrec
    def loop(offset: Int, acc: Vector[Int]): Vector[Int] =
      if offset >= text.length then acc
      else
        val next = TextEditing.nextSubWordBoundary(text, offset)
        loop(next, acc :+ next)

    loop(0, Vector.empty)

  private def backwardStops(text: String): Vector[Int] =
    @tailrec
    def loop(offset: Int, acc: Vector[Int]): Vector[Int] =
      if offset <= 0 then acc
      else
        val previous = TextEditing.previousSubWordBoundary(text, offset)
        loop(previous, acc :+ previous)

    loop(text.length, Vector.empty)

  "Sub-word motion over camelCase" should "stop at each capital that follows a lower-case letter" in {
    forwardStops("fooBarBaz qux") shouldBe Vector(3, 6, 10, 13)
    backwardStops("fooBarBaz qux") shouldBe Vector(10, 6, 3, 0)
  }

  "Sub-word motion over PascalCase" should "stop before the last capital of an acronym" in {
    forwardStops("parseXMLHttpRequest") shouldBe Vector(5, 8, 12, 19)
    backwardStops("parseXMLHttpRequest") shouldBe Vector(12, 8, 5, 0)
  }

  "Sub-word motion over snake_case" should "keep the underscores with the part before them" in {
    forwardStops("snake_case_name x") shouldBe Vector(6, 11, 16, 17)
    backwardStops("snake_case_name x") shouldBe Vector(16, 11, 6, 0)
  }

  it should "keep a run of underscores together" in {
    forwardStops("foo__bar") shouldBe Vector(5, 8)
    backwardStops("foo__bar") shouldBe Vector(5, 0)
  }

  it should "keep leading and trailing underscores with the part beside them" in {
    forwardStops("__init__") shouldBe Vector(8)
    forwardStops("_privateField") shouldBe Vector(8, 13)
    backwardStops("_privateField") shouldBe Vector(8, 0)
  }

  "Sub-word motion over digits" should "keep them with the lower-case letters before them and end the part at a capital" in {
    forwardStops("utf8Decoder v2") shouldBe Vector(4, 12, 14)
  }

  "Sub-word motion outside identifiers" should "move as whole-word motion does" in {
    val text = "The quick, brown fox... jumps!"

    forwardStops(text) shouldBe Vector(4, 9, 11, 17, 20, 24, 29, 30)
    backwardStops(text) shouldBe Vector(29, 24, 20, 17, 11, 9, 4, 0)
    forwardStops("foo.bar") shouldBe Vector(3, 4, 7)
  }

  "Sub-word motion from inside a part" should "go to the end or start of that part" in {
    TextEditing.nextSubWordBoundary("fooBarBaz", 4) shouldBe 6
    TextEditing.previousSubWordBoundary("fooBarBaz", 5) shouldBe 3
    TextEditing.previousSubWordBoundary("fooBarBaz", 3) shouldBe 0
  }

  "Sub-word motion over mixed scripts" should "stop at the dictionary words after the identifier part" in {
    forwardStops("fooBar我们今天") shouldBe Vector(3, 6, 8, 10)
    backwardStops("fooBar我们今天") shouldBe Vector(8, 6, 3, 0)
  }

  it should "treat an emoji sequence as one stop" in {
    val family = "👨‍👩‍👧"

    forwardStops(s"fooBar$family") shouldBe Vector(3, 6, 6 + family.length)
    backwardStops(s"fooBar$family") shouldBe Vector(6, 3, 0)
  }

  "Sub-word motion at the edges" should "stay put at the start and end of the text" in {
    TextEditing.previousSubWordBoundary("fooBar", 0) shouldBe 0
    TextEditing.nextSubWordBoundary("fooBar", 6) shouldBe 6
    TextEditing.nextSubWordBoundary("", 0) shouldBe 0
  }

  it should "give the same stops over a character source as over the string" in {
    val source: CharacterSource = StringCharacterSource("snakeCase_name")

    TextEditing.nextSubWordBoundary(source, 0) shouldBe 5
    TextEditing.previousSubWordBoundary(source, 14) shouldBe 10
  }
