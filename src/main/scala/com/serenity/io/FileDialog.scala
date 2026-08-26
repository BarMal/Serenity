package com.serenity.io

import java.nio.file.Path

import cats.effect.IO

/** A native file chooser. Absence of one (`None` wherever this is threaded, rather than an instance of this trait)
  * means the environment has no native dialog to show at all -- callers fall back to the in-app save-as/open form
  * instead of attempting a dialog that could never appear. That is a different case from a dialog being shown and the
  * user cancelling it, which `chooseOpenFile`/`chooseSaveFile` still report as `None` here.
  */
trait FileDialog:
  def chooseOpenFile(initialDirectory: Option[Path]): IO[Option[Path]]

  def chooseSaveFile(initialDirectory: Option[Path], suggestedFileName: Option[String]): IO[Option[Path]]
