package com.serenity.state.models

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ImeNoticesSpec extends AnyFlatSpec with Matchers:

  "ImeNotices.refused" should "say why the input method was stopped, as a notice that clears itself" in {
    ImeNotices.refused.message should include("block")
    ImeNotices.refused.level should not be NoticeLevel.Error
    ImeNotices.refused.autoDismissAfter should not be empty
  }
