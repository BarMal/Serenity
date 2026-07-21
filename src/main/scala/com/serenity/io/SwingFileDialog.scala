package com.serenity.io

import java.awt.{FileDialog as AwtFileDialog, *}
import java.io.File
import java.nio.file.Path
import java.util.concurrent.atomic.AtomicReference
import javax.swing.{JFileChooser, SwingUtilities}

import scala.util.control.NonFatal

import cats.effect.IO
import com.sun.jna.*
import com.sun.jna.platform.win32.COM.Unknown
import com.sun.jna.platform.win32.{Guid, Ole32}
import com.sun.jna.ptr.{IntByReference, PointerByReference}

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
            case SwingFileDialog.Backend.WindowsModern =>
              nativeOwner match
                case Some(owner) =>
                  SwingFileDialog.chooseWithModernWindowsDialog(owner, mode, initialDirectory, suggestedFileName)
                case None => SwingFileDialog.chooseWithSwingChooser(initialDirectory, suggestedFileName, showDialog)
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
    case WindowsModern
    case Native
    case SwingChooser

  private enum NativeDialogOwner:
    case FrameOwner(frame: Frame)
    case DialogOwner(dialog: Dialog)

  private[io] def preferredBackend(
    hasNativeOwner: Boolean,
    osName: String = System.getProperty("os.name", "")
  ): Backend =
    if !hasNativeOwner then Backend.SwingChooser
    else if WindowsCommonFileDialog.isSupported(osName) then Backend.WindowsModern
    else Backend.Native

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

  private def chooseWithModernWindowsDialog(
    owner: NativeDialogOwner,
    mode: Int,
    initialDirectory: Option[Path],
    suggestedFileName: Option[String]
  ): Option[Path] =
    WindowsCommonFileDialog
      .choose(
        owner = nativePointer(owner),
        mode = mode,
        initialDirectory = initialDirectory,
        suggestedFileName = suggestedFileName
      )
      .fold(
        _ => chooseWithNativeDialog(owner, mode, initialDirectory, suggestedFileName),
        identity
      )

  private def nativePointer(owner: NativeDialogOwner): Pointer =
    owner match
      case NativeDialogOwner.FrameOwner(frame)   => Native.getComponentPointer(frame)
      case NativeDialogOwner.DialogOwner(dialog) => Native.getComponentPointer(dialog)

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

  /** Windows Vista+ Common Item Dialog bridge, with AWT fallback on any COM failure. */
  private object WindowsCommonFileDialog:

    private val ClassContextInprocServer = 1
    private val ForceFileSystem          = 0x40
    private val SigdnFileSystemPath      = 0x80058000
    private val ErrorCancelled           = 0x800704c7

    private val FileOpenDialogClassId = new Guid.CLSID("{DC1C5A9C-E88A-4DDE-A5A1-60F82A20AEF7}")
    private val FileSaveDialogClassId = new Guid.CLSID("{C0B4E2F3-BA21-4773-8DBA-335EC946EB8B}")
    private val FileOpenDialogId      = new Guid.IID("{D57C7288-D4AD-4768-BE02-9D969532D960}")
    private val FileSaveDialogId      = new Guid.IID("{84BCCD23-5FDE-4CDB-AEA4-AF64B83D78AB}")
    private val ShellItemId           = new Guid.IID("{43826D1E-E718-42EE-BC55-A1E261C37BFE}")

    def isSupported(osName: String = System.getProperty("os.name", "")): Boolean =
      osName.toLowerCase(java.util.Locale.ROOT).contains("windows")

    def choose(
      owner: Pointer,
      mode: Int,
      initialDirectory: Option[Path],
      suggestedFileName: Option[String]
    ): Either[Unit, Option[Path]] =
      try
        val initialization = Ole32.INSTANCE.CoInitializeEx(Pointer.NULL, Ole32.COINIT_APARTMENTTHREADED).intValue()
        if failed(initialization) then Left(())
        else
          try chooseInitialized(owner, mode, initialDirectory, suggestedFileName)
          finally Ole32.INSTANCE.CoUninitialize()
      catch case NonFatal(_) => Left(())

    private def chooseInitialized(
      owner: Pointer,
      mode: Int,
      initialDirectory: Option[Path],
      suggestedFileName: Option[String]
    ): Either[Unit, Option[Path]] =
      val dialogReference = new PointerByReference()
      val createResult = Ole32.INSTANCE
        .CoCreateInstance(
          if mode == AwtFileDialog.SAVE then FileSaveDialogClassId else FileOpenDialogClassId,
          Pointer.NULL,
          ClassContextInprocServer,
          if mode == AwtFileDialog.SAVE then FileSaveDialogId else FileOpenDialogId,
          dialogReference
        )
        .intValue()
      if failed(createResult) then Left(())
      else
        val dialog = new CommonFileDialog(dialogReference.getValue)
        try
          val configured = forceFileSystem(dialog) &&
            suggestedFileName.forall(name => succeeded(dialog.setFileName(name))) &&
            initialDirectory.forall(directory => setInitialDirectory(dialog, directory))
          if !configured then Left(())
          else
            dialog.show(owner) match
              case result if result == ErrorCancelled => Right(None)
              case result if failed(result)           => Left(())
              case _                                  => selectedPath(dialog)
        finally
          val _ = dialog.Release()

    private def forceFileSystem(dialog: CommonFileDialog): Boolean =
      val options = new IntByReference()
      succeeded(dialog.getOptions(options)) && succeeded(dialog.setOptions(options.getValue | ForceFileSystem))

    private def setInitialDirectory(dialog: CommonFileDialog, directory: Path): Boolean =
      val itemReference = new PointerByReference()
      val createItem = NativeLibrary
        .getInstance("shell32")
        .getFunction("SHCreateItemFromParsingName")
        .invokeInt(
          Array(new WString(directory.normalize().toAbsolutePath.toString), Pointer.NULL, ShellItemId, itemReference)
        )
      if failed(createItem) then false
      else
        val item = new Unknown(itemReference.getValue)
        try succeeded(dialog.setFolder(item.getPointer))
        finally
          val _ = item.Release()

    private def selectedPath(dialog: CommonFileDialog): Either[Unit, Option[Path]] =
      val itemReference = new PointerByReference()
      if failed(dialog.getResult(itemReference)) then Left(())
      else
        val item = new ShellItem(itemReference.getValue)
        try
          val pathReference = new PointerByReference()
          if failed(item.getDisplayName(pathReference)) then Left(())
          else
            val pathPointer = pathReference.getValue
            try Right(Option(pathPointer).map(pointer => Path.of(pointer.getWideString(0)).normalize()))
            finally if pathPointer != null then Ole32.INSTANCE.CoTaskMemFree(pathPointer)
        finally
          val _ = item.Release()

    private def succeeded(result: Int): Boolean = !failed(result)
    private def failed(result: Int): Boolean    = result < 0

    final private class CommonFileDialog(pointer: Pointer) extends Unknown(pointer):
      def show(owner: Pointer): Int =
        _invokeNativeInt(3, Array(getPointer, owner))

      def setFolder(folder: Pointer): Int =
        _invokeNativeInt(12, Array(getPointer, folder))

      def setOptions(options: Int): Int =
        _invokeNativeInt(9, Array(getPointer, Int.box(options)))

      def getOptions(options: IntByReference): Int =
        _invokeNativeInt(10, Array(getPointer, options))

      def setFileName(name: String): Int =
        _invokeNativeInt(15, Array(getPointer, new WString(name)))

      def getResult(result: PointerByReference): Int =
        _invokeNativeInt(20, Array(getPointer, result))

    final private class ShellItem(pointer: Pointer) extends Unknown(pointer):
      def getDisplayName(result: PointerByReference): Int =
        _invokeNativeInt(5, Array(getPointer, Int.box(SigdnFileSystemPath), result))
