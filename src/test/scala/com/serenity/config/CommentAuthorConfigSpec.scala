package com.serenity.config

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CommentAuthorConfigSpec extends AnyFlatSpec with Matchers:

  private val settingKey = "document.comment_author"

  private def read(value: String): AppConfig =
    ConfigRegistry.read(AppConfig.default, settingKey, value).getOrElse(fail(s"$settingKey = $value was refused"))

  "document.comment_author" should "be registered and written to the config file" in {
    ConfigRegistry.find(settingKey) should not be empty
    ConfigFileFormat.settings(AppConfig.default).map(_._1) should contain(settingKey)
  }

  it should "default to the operating system's user name" in {
    AppConfig.default.documentConfig.commentAuthor shouldBe None
    AppConfig.default.commentAuthor shouldBe CommentAuthor.osUserName
    AppConfig.default.commentAuthor should not be empty
  }

  it should "name the author comments are written under" in {
    read("Ada Lovelace").commentAuthor shouldBe "Ada Lovelace"
  }

  it should "go back to the operating system's user name when set to auto" in {
    val named = read("Ada")

    ConfigRegistry.read(named, settingKey, "auto").map(_.commentAuthor) shouldBe Some(CommentAuthor.osUserName)
  }

  it should "treat a blank name as auto rather than store it" in {
    ConfigRegistry.read(read("Ada"), settingKey, "   ").map(_.documentConfig.commentAuthor) shouldBe Some(None)
  }
