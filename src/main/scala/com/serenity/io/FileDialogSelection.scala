package com.serenity.io

import java.util.Locale

import com.serenity.config.FileDialogMode

/** Decides, once at startup, whether open and save use the system's file dialog or Swing's own chooser. */
object FileDialogSelection:

  final case class Decision(useNative: Boolean, reason: String):
    def description: String = s"${if useNative then "native dialog" else "Swing chooser"} ($reason)"

  def forThisProcess(mode: FileDialogMode): Decision =
    decide(mode, System.getProperty("os.name", ""), GtkFileChooserSchema.installedForThisProcess)

  /** The schema probe is by-name so that only Linux in auto mode pays for reading the compiled schema file. */
  def decide(mode: FileDialogMode, osName: String, gtkSchemaInstalled: => Boolean): Decision =
    mode match
      case FileDialogMode.Swing  => Decision(useNative = false, "ui.file_dialog = swing")
      case FileDialogMode.Native => Decision(useNative = true, "ui.file_dialog = native")
      case FileDialogMode.Auto if !osName.toLowerCase(Locale.ROOT).contains("linux") =>
        Decision(useNative = true, s"$osName has a system dialog that needs no GSettings schema")
      case FileDialogMode.Auto if gtkSchemaInstalled =>
        Decision(useNative = true, s"${GtkFileChooserSchema.SchemaId} is installed")
      case FileDialogMode.Auto =>
        Decision(
          useNative = false,
          s"${GtkFileChooserSchema.SchemaId} is not installed and GTK would abort the process; " +
            "ui.file_dialog = native overrides"
        )
