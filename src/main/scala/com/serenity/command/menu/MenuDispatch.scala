package com.serenity.command.menu

import java.nio.file.Path

import com.serenity.command.{CommandId, CommandKeyBindings}
import com.serenity.keystroke.events.{ActivateBuffer, Event, OpenRecentPath, RunCommand}
import com.serenity.keystroke.translators.TextHotkeyConverters
import com.serenity.state.models.BufferId

object MenuDispatch:

  /** What a dynamic menu entry stands for once the shell has filled it in. */
  enum Choice:
    case Buffer(id: BufferId)
    case RecentFile(path: Path)

  /** The event of the hotkey action that performs the command, so a menu Paste behaves exactly like the key (clipboard
    * sync included); otherwise `RunCommand`, which applies the same availability gate as a key bound to the command.
    */
  def eventFor(id: CommandId): Event =
    actionEventByCommand.getOrElse(id, RunCommand(id.value))

  def eventFor(choice: Choice): Event =
    choice match
      case Choice.Buffer(id)       => ActivateBuffer(id)
      case Choice.RecentFile(path) => OpenRecentPath(path)

  /** Only a command item has an event of its own: the other entries lay out the menu or need a [[Choice]] first. */
  def eventFor(entry: ResolvedEntry): Option[Event] =
    entry match
      case ResolvedEntry.Item(command) => Some(eventFor(CommandId(command.name)))
      case _                           => None

  private val actionEventByCommand: Map[CommandId, Event] =
    TextHotkeyConverters.actionEvents
      .flatMap((action, event) => CommandKeyBindings.commandFor(action).map(_ -> event))
      .groupMapReduce(_._1)(_._2)((first, _) => first)
