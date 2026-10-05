package com.serenity.richtext

import java.nio.charset.StandardCharsets

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RtfSyntaxSpec extends AnyFlatSpec with Matchers:

  private def bytes(rtf: String): Array[Byte] = rtf.getBytes(StandardCharsets.ISO_8859_1)

  "RtfTokenizer" should "split control words, parameters, delimiter spaces, groups and text" in {
    RtfTokenizer.tokenize(bytes("""{\rtf1\ansi \b bold\b0  x}""")) shouldBe Right(
      Vector(
        RtfToken.GroupStart,
        RtfToken.Word("rtf", Some(1)),
        RtfToken.Word("ansi", None),
        RtfToken.Word("b", None),
        RtfToken.Text("bold"),
        RtfToken.Word("b", Some(0)),
        RtfToken.Text(" x"),
        RtfToken.GroupEnd
      )
    )
  }

  it should "read negative parameters and hex escapes" in {
    RtfTokenizer.tokenize(bytes("""\u-3913 ?\'e9""")) shouldBe Right(
      Vector(RtfToken.Word("u", Some(-3913)), RtfToken.Text("?"), RtfToken.HexByte(0xe9))
    )
  }

  it should "treat a backslash-newline as a paragraph break and ignore bare line endings" in {
    RtfTokenizer.tokenize(bytes("a\r\nb\\\nc")) shouldBe Right(
      Vector(RtfToken.Text("a"), RtfToken.Text("b"), RtfToken.Word("par", None), RtfToken.Text("c"))
    )
  }

  it should "skip the raw bytes announced by \\bin without interpreting them" in {
    RtfTokenizer.tokenize(bytes("""\bin3 {}\ab""")) shouldBe Right(Vector(RtfToken.Text("ab")))
  }

  it should "fail on a truncated escape" in {
    RtfTokenizer.tokenize(bytes("""abc\""")).isLeft shouldBe true
    RtfTokenizer.tokenize(bytes("""abc\'4""")).isLeft shouldBe true
    RtfTokenizer.tokenize(bytes("""\bin9 ab""")).isLeft shouldBe true
  }

  "RtfParser" should "build nested groups and turn escaped specials into text" in {
    RtfParser.parse(bytes("""{\rtf1 a\{{\b b}}""")) shouldBe Right(
      RtfNode.Group(
        Vector(
          RtfNode.Control("rtf", Some(1)),
          RtfNode.Text("a"),
          RtfNode.Text("{"),
          RtfNode.Group(Vector(RtfNode.Control("b", None), RtfNode.Text("b")))
        )
      )
    )
  }

  it should "reject unbalanced braces, content that is not RTF and absurd nesting" in {
    RtfParser.parse(bytes("""{\rtf1 {\b bold}""")).isLeft shouldBe true
    RtfParser.parse(bytes("""{\rtf1 a}}""")).isLeft shouldBe true
    RtfParser.parse(bytes("""plain text""")).isLeft shouldBe true
    RtfParser.parse(bytes("")).isLeft shouldBe true
    RtfParser.parse(bytes("{" * 100000)).isLeft shouldBe true
  }

  it should "ignore bytes after the root group such as trailing line endings or NULs" in {
    RtfParser.parse(bytes("{\\rtf1 a}\r\n\u0000")).isRight shouldBe true
  }
