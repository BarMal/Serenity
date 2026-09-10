package com.serenity.command

import com.serenity.config.AppConfig
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** `CommandRunner.commandBindings` hand-maintains a `Map[String, HotkeyAction]` keyed by command-name string
  * literals that must stay in sync with the `name`s declared independently across the `CommandRegistry*Commands`
  * files (issue #1426). Nothing links a key here to a registered `Command` at compile time, so a rename on either
  * side would silently desync; this test catches that instead.
  */
class CommandRunnerCommandBindingsSpec extends AnyFlatSpec with Matchers:

  "CommandRunner.commandBindings" should "only key on command names that are currently registered" in {
    val registeredNames = CommandRegistry.default.getAllCommands.map(_.name).toSet
    val boundNames      = CommandRunner.commandBindings(AppConfig.default).keySet

    boundNames.diff(registeredNames) shouldBe Set.empty
  }

