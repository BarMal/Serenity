package com.serenity

import com.serenity.command.CommandRegistry
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CommandRegistryCachingSpec extends AnyFlatSpec with Matchers:

  "CommandRegistry.default" should "reuse the same instance across accesses instead of rebuilding the command list" in {
    CommandRegistry.default should be theSameInstanceAs CommandRegistry.default
  }

  "CommandRegistry.withToggleUI" should "reuse the same instance across accesses instead of rebuilding the command list" in {
    CommandRegistry.withToggleUI should be theSameInstanceAs CommandRegistry.withToggleUI
  }
