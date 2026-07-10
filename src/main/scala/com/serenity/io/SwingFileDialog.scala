package com.serenity.io

import java.awt.{FileDialog as AwtFileDialog, *}
import java.io.File
import java.nio.file.Path
import java.util.concurrent.atomic.AtomicReference
import javax.swing.{JFileChooser, SwingUtilities}

import cats.effect.IO

class SwingFileDialog(parent: Component) extends FileDialog:

  override def chooseOpenFile(initialDirectory: Option[Path]): IO[Option[Path]] =
    choose(AwtFileDialog.LOAD, initialDirectory, None, _.showOpenDialog(parent))

  override def chooseSaveFile(initialDirectory: Option[Path], suggestedFileName: Option[String]): IO[Option[Path]] =
    choose(AwtFileDialog.SAVE, initialDirectory, suggestedFileName, _.showSaveDialog(parent))

  private def choose(
    mode: Int,
    initialDirectory: Option[Path],
    suggestedFileName: Option[String],
    showDialog: JFileChooser => Int
  ): IO[Option[Path]] =
    IO.blocking {
      val selectedPath = new AtomicReference[Option[Path]](None)
      val runnable: Runnable = () =>
        val nativeOwner = SwingFileDialog.nativeDialogOwner(parent)
        val result =
          SwingFileDialog.preferredBackend(nativeOwner.isDefined) match
            case SwingFileDialog.Backend.Native =>
              nativeOwner.flatMap(owner =>
                SwingFileDialog.chooseWithNativeDialog(owner, mode, initialDirectory, suggestedFileName)
              )
            case SwingFileDialog.Backend.SwingChooser =>
              SwingFileDialog.chooseWithSwingChooser(initialDirectory, suggestedFileName, showDialog)
        selectedPath.set(result)

      if SwingUtilities.isEventDispatchThread then runnable.run()
      else SwingUtilities.invokeAndWait(runnable)

      selectedPath.get()
    }

object SwingFileDialog:

  enum Backend:
    case Native
    case SwingChooser

  private enum NativeDialogOwner:
    case FrameOwner(frame: Frame)
    case DialogOwner(dialog: Dialog)

  private[io] def preferredBackend(hasNativeOwner: Boolean): Backend =
    if hasNativeOwner then Backend.Native else Backend.SwingChooser

  private[io] def normalizeNativeSelection(directory: String | Null, file: String | Null): Option[Path] =
    Option(file).map { selectedFile =>
      Option(directory)
        .filter(_.nonEmpty)
        .map(dir => Path.of(dir, selectedFile))
        .getOrElse(Path.of(selectedFile))
        .normalize()
    }

  private[io] def normalizeSwingSelection(file: File | Null): Option[Path] =
    Option(file).map(_.toPath.normalize())

  private def nativeDialogOwner(parent: Component): Option[NativeDialogOwner] =
    Option(SwingUtilities.getWindowAncestor(parent)).flatMap {
      case frame: Frame   => Some(NativeDialogOwner.FrameOwner(frame))
      case dialog: Dialog => Some(NativeDialogOwner.DialogOwner(dialog))
      case _              => None
    }

  private def chooseWithNativeDialog(
    owner: NativeDialogOwner,
    mode: Int,
    initialDirectory: Option[Path],
    suggestedFileName: Option[String]
  ): Option[Path] =
    val dialog = owner match
      case NativeDialogOwner.FrameOwner(frame)   => AwtFileDialog(frame, dialogTitle(mode), mode)
      case NativeDialogOwner.DialogOwner(dialog) => AwtFileDialog(dialog, dialogTitle(mode), mode)

    try
      initialDirectory.foreach(path => dialog.setDirectory(path.normalize().toAbsolutePath.toString))
      suggestedFileName.foreach(dialog.setFile)
      dialog.setVisible(true)
      normalizeNativeSelection(dialog.getDirectory, dialog.getFile)
    finally dialog.dispose()

  private def chooseWithSwingChooser(
    initialDirectory: Option[Path],
    suggestedFileName: Option[String],
    showDialog: JFileChooser => Int
  ): Option[Path] =
    val chooser = new JFileChooser()
    initialDirectory.foreach(path => chooser.setCurrentDirectory(path.toFile))
    suggestedFileName.foreach(name => chooser.setSelectedFile(File(name)))
    val result = showDialog(chooser)
    if result == JFileChooser.APPROVE_OPTION then normalizeSwingSelection(chooser.getSelectedFile)
    else None

  private def dialogTitle(mode: Int): String =
    if mode == AwtFileDialog.SAVE then "Save File"
    else "Open File"
