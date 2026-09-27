package com.serenity.command

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Issue #1693: `CommandRunner.commandUsage` and `Persisted.commandUsage` used to be keyed by a bare `String` (a
  * `Command.name`), the same shape as any other `String`-keyed map in `state/models` an opaque id exists to rule out.
  */
class CommandIdSpec extends AnyFlatSpec with Matchers:

  "apply/value" should "round-trip the underlying name" in {
    CommandId("save-session").value shouldBe "save-session"
  }

  "two CommandIds built from the same name" should "compare equal" in {
    CommandId("save-session") shouldBe CommandId("save-session")
  }

  "two CommandIds built from different names" should "compare unequal" in {
    CommandId("save-session") should not be CommandId("open-session")
  }

  "unapply" should "extract the underlying name for pattern matching" in {
    CommandId("save-session") match
      case CommandId(name) => name shouldBe "save-session"
  }
