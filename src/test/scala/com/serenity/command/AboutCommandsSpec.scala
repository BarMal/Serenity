package com.serenity.command

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AboutCommandsSpec extends AnyFlatSpec with Matchers:

  private def intentOf(id: String): Option[CommandIntent] = CommandRegistry.default.findCommand(id).map(_.intent)

  "The about command" should "open the About Serenity document" in {
    intentOf("about") shouldBe Some(CommandIntent.File(FileIntent.ShowAbout))
    CommandRegistry.default.findCommand("about").map(_.label) shouldBe Some("About Serenity")
  }

  "The open-releases-page command" should "open the releases page" in {
    intentOf("open-releases-page") shouldBe Some(CommandIntent.File(FileIntent.OpenReleasesPage))
    CommandRegistry.default.findCommand("open-releases-page").map(_.label) shouldBe Some("Open Releases Page")
  }

  "The show-licence-and-notices command" should "remain registered as an alias" in {
    intentOf("show-licence-and-notices") shouldBe Some(CommandIntent.File(FileIntent.ShowLicenceAndNotices))
  }
