package com.serenity.io

import java.nio.file.Path

import cats.effect.IO

/** A native file chooser. Absence of one (`None` wherever this is threaded, rather than an instance of this record)
  * means the environment has no native dialog to show at all -- callers fall back to the in-app save-as/open form
  * instead of attempting a dialog that could never appear. That is a different case from a dialog being shown and the
  * user cancelling it, which `chooseOpenFile`/`chooseSaveFile` still report as `None` here.
  *
  * A cold capability (user-initiated, not a per-frame/per-glyph boundary) expressed as a record of functions rather
  * than a trait -- see #1017. A test double is a record literal, not a subclass; wrapping one in logging or retry is
  * `copy(chooseOpenFile = ...)`.
  */
final case class FileDialog(
    chooseOpenFile: Option[Path] => IO[Option[Path]],
    chooseSaveFile: (Option[Path], Option[String]) => IO[Option[Path]]
)
