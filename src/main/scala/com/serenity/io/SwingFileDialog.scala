package com.serenity.io

import java.awt.{FileDialog as AwtFileDialog, *}
import java.io.File
import java.lang.reflect.InvocationTargetException
import java.nio.file.Path
import java.util.concurrent.{ExecutionException, FutureTask}
import javax.swing.{JFileChooser, SwingUtilities}

import scala.util.Try
import scala.util.control.NonFatal

import cats.effect.IO
import com.sun.jna.*
import com.sun.jna.platform.win32.COM.Unknown
import com.sun.jna.platform.win32.{Guid, Ole32}
import com.sun.jna.ptr.{IntByReference, PointerByReference}

object SwingFileDialog:

  def apply(parent: Component): FileDialog =
    FileDialog(
      chooseOpenFile =
        initialDirectory => choose(parent, AwtFileDialog.LOAD, initialDirectory, None, _.showOpenDialog(parent)),
      chooseSaveFile = (initialDirectory, suggestedFileName) =>
        choose(parent, AwtFileDialog.SAVE, initialDirectory, suggestedFileName, _.showSaveDialog(parent)),
      chooseFolder = initialDirectory => chooseFolder(parent, initialDirectory),
      chooseFileOrFolder = AppKitOpenPanel.choose,
      supportsFileOrFolder = AppKitOpenPanel.isAvailable(System.getProperty("os.name", ""))
    )

  // Runs `body` on the event dispatch thread and waits for it. Dialogs, and the global state they read as they
  // open, are only touched there. While one dialog is modal, a second request runs inside its event loop rather
  // than alongside it, so the changes made to that state nest.
  private[io] def onEventThread[A](body: => A): IO[A] =
    IO.blocking(
      runOnEventThread(SwingUtilities.isEventDispatchThread, SwingUtilities.invokeAndWait(_))(body)
    ).flatMap(IO.fromEither)

  // The task carries the result, or the failure, back from the dispatch thread. Failures are handed back as the
  // exception `body` raised, not as the wrapper the task or `invokeAndWait` puts around it.
  private[io] def runOnEventThread[A](onDispatchThread: Boolean, invokeAndWait: Runnable => Unit)(
    body: => A
  ): Either[Throwable, A] =
    val task = new FutureTask[A](() => body)
    Try {
      if onDispatchThread then task.run() else invokeAndWait(task)
      task.get()
    }.toEither.left.map(originalCause)

  private def originalCause(failure: Throwable): Throwable =
    failure match
      case wrapper @ (_: ExecutionException | _: InvocationTargetException) =>
        Option(wrapper.getCause).getOrElse(wrapper)
      case other => other

  private def choose(
    parent: Component,
    mode: Int,
    initialDirectory: Option[Path],
    suggestedFileName: Option[String],
    showDialog: JFileChooser => Int
  ): IO[Option[Path]] =
    onEventThread {
      val nativeOwner = SwingFileDialog.nativeDialogOwner(parent)
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
    }

  /** Folders are picked where the platform can: the Windows Common Item Dialog and the macOS AWT dialog. Linux's AWT
    * dialog has no directory mode, so it takes the Swing chooser, which does.
    */
  private def chooseFolder(parent: Component, initialDirectory: Option[Path]): IO[Option[Path]] =
    onEventThread {
      val nativeOwner = SwingFileDialog.nativeDialogOwner(parent)
      SwingFileDialog.preferredFolderBackend(nativeOwner.isDefined) match
        case SwingFileDialog.Backend.WindowsModern =>
          nativeOwner.flatMap(owner =>
            WindowsCommonFileDialog
              .choose(nativePointer(owner), AwtFileDialog.LOAD, initialDirectory, None, pickFolders = true)
              .fold(_ => chooseFolderWithSwingChooser(parent, initialDirectory), identity)
          )
        case SwingFileDialog.Backend.Native =>
          nativeOwner.flatMap(owner =>
            chooseWithNativeDialog(owner, AwtFileDialog.LOAD, initialDirectory, None, directories = true)
          )
        case SwingFileDialog.Backend.SwingChooser =>
          chooseFolderWithSwingChooser(parent, initialDirectory)
    }

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

  private[io] val ForceFileSystem = 0x40
  private[io] val PickFolders     = 0x20
  private val FolderDialogTitle   = "Open Folder"

  private[io] def preferredFolderBackend(
    hasNativeOwner: Boolean,
    osName: String = System.getProperty("os.name", "")
  ): Backend =
    if !hasNativeOwner then Backend.SwingChooser
    else if WindowsCommonFileDialog.isSupported(osName) then Backend.WindowsModern
    else if AppleDirectoryMode.appliesTo(osName) then Backend.Native
    else Backend.SwingChooser

  private[io] def windowsDialogOptions(current: Int, pickFolders: Boolean): Int =
    current | ForceFileSystem | (if pickFolders then PickFolders else 0)

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
    suggestedFileName: Option[String],
    directories: Boolean = false
  ): Option[Path] =
    val title = if directories then FolderDialogTitle else dialogTitle(mode)
    val dialog = owner match
      case NativeDialogOwner.FrameOwner(frame)   => AwtFileDialog(frame, title, mode)
      case NativeDialogOwner.DialogOwner(dialog) => AwtFileDialog(dialog, title, mode)

    try
      initialDirectory.foreach(path => dialog.setDirectory(path.normalize().toAbsolutePath.toString))
      suggestedFileName.foreach(dialog.setFile)
      if AppleDirectoryMode.appliesTo(System.getProperty("os.name", "")) then
        AppleDirectoryMode.around(directories)(dialog.setVisible(true))
      else dialog.setVisible(true)
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

  private def chooseFolderWithSwingChooser(parent: Component, initialDirectory: Option[Path]): Option[Path] =
    val chooser = new JFileChooser()
    chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY)
    chooser.setDialogTitle(FolderDialogTitle)
    initialDirectory.foreach(path => chooser.setCurrentDirectory(path.toFile))
    if chooser.showOpenDialog(parent) == JFileChooser.APPROVE_OPTION then
      normalizeSwingSelection(chooser.getSelectedFile)
    else None

  private def dialogTitle(mode: Int): String =
    if mode == AwtFileDialog.SAVE then "Save File"
    else "Open File"

  /** Windows Vista+ Common Item Dialog bridge, with AWT fallback on any COM failure. */
  private object WindowsCommonFileDialog:

    private val ClassContextInprocServer = 1
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
      suggestedFileName: Option[String],
      pickFolders: Boolean = false
    ): Either[Unit, Option[Path]] =
      try
        val initialization = Ole32.INSTANCE.CoInitializeEx(Pointer.NULL, Ole32.COINIT_APARTMENTTHREADED).intValue()
        if failed(initialization) then Left(())
        else
          try chooseInitialized(owner, mode, initialDirectory, suggestedFileName, pickFolders)
          finally Ole32.INSTANCE.CoUninitialize()
      catch case NonFatal(_) => Left(())

    private def chooseInitialized(
      owner: Pointer,
      mode: Int,
      initialDirectory: Option[Path],
      suggestedFileName: Option[String],
      pickFolders: Boolean
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
          val configured = configureOptions(dialog, pickFolders) &&
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

    private def configureOptions(dialog: CommonFileDialog, pickFolders: Boolean): Boolean =
      val options = new IntByReference()
      succeeded(dialog.getOptions(options)) &&
      succeeded(dialog.setOptions(windowsDialogOptions(options.getValue, pickFolders)))

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
