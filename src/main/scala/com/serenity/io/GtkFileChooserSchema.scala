package com.serenity.io

import java.nio.charset.StandardCharsets.ISO_8859_1
import java.nio.file.{Files, Path}

import scala.util.Try

/** Whether GLib can find the `org.gtk.Settings.FileChooser` GSettings schema.
  *
  * The JDK's GTK file dialog creates a `GtkFileChooserDialog`, and GTK then reads that schema. GLib has no recoverable
  * error for a missing one: `g_settings_new` aborts the whole process (`GLib-GIO-ERROR`, SIGABRT), so the JVM cannot
  * catch it and has to know beforehand. GLib looks in `GSETTINGS_SCHEMA_DIR`, then `$XDG_DATA_HOME` and
  * `$XDG_DATA_DIRS`, each under `glib-2.0/schemas/gschemas.compiled`.
  *
  * The probe errs towards "not installed": a false negative costs a Swing dialog, a false positive costs the process.
  * It looks for the schema id's text in the compiled file rather than parsing GVDB, and does not know of directories
  * GLib was compiled to search that are not on those variables.
  */
object GtkFileChooserSchema:

  val SchemaId: String = "org.gtk.Settings.FileChooser"

  private val CompiledFileName   = "gschemas.compiled"
  private val SchemaSubdirectory = Path.of("glib-2.0", "schemas")

  /** Resolved once, since neither the environment nor the installed schemas change under a running editor. */
  lazy val installedForThisProcess: Boolean =
    isInstalled(sys.env, Path.of(System.getProperty("user.home", "")))

  def isInstalled(env: Map[String, String], userHome: Path): Boolean =
    searchDirectories(env, userHome).exists(directory => declaresSchema(directory.resolve(CompiledFileName)))

  def searchDirectories(env: Map[String, String], userHome: Path): List[Path] =
    def value(name: String): Option[String] = env.get(name).map(_.trim).filter(_.nonEmpty)
    def entries(text: String): List[Path]   = text.split(':').toList.filter(_.nonEmpty).map(Path.of(_))

    val explicit = value("GSETTINGS_SCHEMA_DIR").toList.flatMap(entries)
    val userData = value("XDG_DATA_HOME").map(Path.of(_)).getOrElse(userHome.resolve(".local").resolve("share"))
    val systemData =
      value("XDG_DATA_DIRS").map(entries).getOrElse(List(Path.of("/usr/local/share"), Path.of("/usr/share")))
    explicit ++ (userData :: systemData).map(_.resolve(SchemaSubdirectory))

  private def declaresSchema(compiled: Path): Boolean =
    Try {
      Files.isRegularFile(compiled) && String(Files.readAllBytes(compiled), ISO_8859_1).contains(SchemaId)
    }.getOrElse(false)
