package com.serenity.command

/** A [[Command]]'s `name`, distinct from any other `String` a command-runner map is keyed by (issue #1693) --
  * `CommandRunner.commandUsage` and `Persisted.commandUsage` are both MRU-recency maps keyed by this, not by an
  * arbitrary settings-row id or item id (those stay bare `String`s: they aren't a `Command`'s own identity).
  */
opaque type CommandId = String

object CommandId:
  def apply(value: String): CommandId      = value
  def unapply(id: CommandId): Some[String] = Some(id)

  extension (id: CommandId) def value: String = id
