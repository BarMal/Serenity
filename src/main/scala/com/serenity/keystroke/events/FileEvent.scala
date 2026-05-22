package com.serenity.keystroke.events

import java.nio.file.Path

/** Events related to file operations */
trait FileEvent extends Event

// OpenFile and SaveFile are defined in HotkeyEvent.scala
case object SaveAsFile            extends FileEvent
case object OpenFileBrowser       extends FileEvent
case class LoadFile(path: Path)   extends FileEvent
case class SaveFileAs(path: Path) extends FileEvent
