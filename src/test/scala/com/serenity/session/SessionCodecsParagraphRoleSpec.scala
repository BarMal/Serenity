package com.serenity.session

import com.serenity.richtext.ParagraphRole
import io.circe.syntax.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** JSON round trip for [[ParagraphRole]], covering the tagged-object shape (`"type"` plus a role-specific field) each
  * case is persisted as in session files.
  */
class SessionCodecsParagraphRoleSpec extends AnyFlatSpec with Matchers:

  "ParagraphRole codec" should "round trip Body" in {
    val role = ParagraphRole.Body
    role.asJson.as[ParagraphRole] shouldBe Right(role)
  }

  it should "round trip a Heading" in {
    val role = ParagraphRole.Heading(2)
    role.asJson.as[ParagraphRole] shouldBe Right(role)
  }

  it should "round trip a DropCap" in {
    val role = ParagraphRole.DropCap(4)
    role.asJson.as[ParagraphRole] shouldBe Right(role)
    role.asJson.hcursor.downField("type").as[String] shouldBe Right("drop_cap")
    role.asJson.hcursor.downField("lines").as[Int] shouldBe Right(4)
  }

  it should "clamp a decoded drop cap span below one line to one" in {
    val json = io.circe.Json.obj(
      "type"  -> io.circe.Json.fromString("drop_cap"),
      "lines" -> io.circe.Json.fromInt(0)
    )
    json.as[ParagraphRole] shouldBe Right(ParagraphRole.DropCap(1))
  }

  it should "default a drop cap missing its lines field to the conventional span" in {
    val json = io.circe.Json.obj("type" -> io.circe.Json.fromString("drop_cap"))
    json.as[ParagraphRole] shouldBe Right(ParagraphRole.DropCap(ParagraphRole.DefaultDropCapLines))
  }
