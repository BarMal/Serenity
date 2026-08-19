package com.serenity.keystroke.events

import java.nio.file.Path

/** Events related to file operations */
sealed trait FileEvent

case object OpenFile                    extends FileEvent
case object SaveFile                    extends FileEvent
case object SaveAsFile                  extends FileEvent
case object OpenFileBrowser             extends FileEvent
final case class LoadFile(path: Path)   extends FileEvent
final case class SaveFileAs(path: Path) extends FileEvent
