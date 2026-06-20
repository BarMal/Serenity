package com.serenity.io

import java.awt.Component
import java.io.File
import java.nio.file.Path
import java.util.concurrent.atomic.AtomicReference
import javax.swing.{JFileChooser, SwingUtilities}

import cats.effect.IO

class SwingFileDialog(parent: Component) extends FileDialog:

  override def chooseOpenFile(initialDirectory: Option[Path]): IO[Option[Path]] =
    choose(initialDirectory, None, _.showOpenDialog(parent))

  override def chooseSaveFile(initialDirectory: Option[Path], suggestedFileName: Option[String]): IO[Option[Path]] =
    choose(initialDirectory, suggestedFileName, _.showSaveDialog(parent))

  private def choose(
    initialDirectory: Option[Path],
    suggestedFileName: Option[String],
    showDialog: JFileChooser => Int
  ): IO[Option[Path]] =
    IO.blocking {
      val selectedPath = new AtomicReference[Option[Path]](None)
      val runnable: Runnable = () =>
        val chooser = new JFileChooser()
        initialDirectory.foreach(path => chooser.setCurrentDirectory(path.toFile))
        suggestedFileName.foreach(name => chooser.setSelectedFile(File(name)))
        val result = showDialog(chooser)
        if result == JFileChooser.APPROVE_OPTION then
          selectedPath.set(Option(chooser.getSelectedFile).map(_.toPath.normalize()))

      if SwingUtilities.isEventDispatchThread then runnable.run()
      else SwingUtilities.invokeAndWait(runnable)

      selectedPath.get()
    }
