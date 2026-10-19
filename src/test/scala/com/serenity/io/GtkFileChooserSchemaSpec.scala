package com.serenity.io

import java.io.File
import java.nio.charset.StandardCharsets.ISO_8859_1
import java.nio.file.{Files, Path}

import com.serenity.TestTemp
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GtkFileChooserSchemaSpec extends AnyFlatSpec with Matchers:

  private val home = Path.of("/home/someone")

  private def schemaRoot(dataDir: Path): Path = dataDir.resolve("glib-2.0").resolve("schemas")

  private def writeCompiled(directory: Path, schemaIds: String*): Path =
    Files.createDirectories(directory)
    val payload = schemaIds.map(id => s"\u0000\u0001GVariant$id\u0000\u0002").mkString
    Files.write(directory.resolve("gschemas.compiled"), payload.getBytes(ISO_8859_1))

  private val Separator = File.pathSeparator

  private def tempDir(): Path = TestTemp.directory("serenity-gschemas")

  "the GTK FileChooser schema probe" should "find the schema in a compiled file under XDG_DATA_DIRS" in {
    val data = tempDir()
    writeCompiled(schemaRoot(data), "org.gnome.desktop.interface", GtkFileChooserSchema.SchemaId)

    GtkFileChooserSchema.isInstalled(Map("XDG_DATA_DIRS" -> data.toString), home) shouldBe true
  }

  it should "report the schema absent when no compiled file exists" in {
    GtkFileChooserSchema.isInstalled(Map("XDG_DATA_DIRS" -> tempDir().toString), home) shouldBe false
  }

  it should "report the schema absent when the compiled file does not declare it" in {
    val data = tempDir()
    writeCompiled(schemaRoot(data), "org.gnome.desktop.interface", "org.gtk.Settings.Debug")

    GtkFileChooserSchema.isInstalled(Map("XDG_DATA_DIRS" -> data.toString), home) shouldBe false
  }

  it should "not mistake a directory named like the compiled file for a schema source" in {
    val data = tempDir()
    Files.createDirectories(schemaRoot(data).resolve("gschemas.compiled"))

    GtkFileChooserSchema.isInstalled(Map("XDG_DATA_DIRS" -> data.toString), home) shouldBe false
  }

  it should "look through every XDG_DATA_DIRS entry" in {
    val empty = tempDir()
    val full  = tempDir()
    writeCompiled(schemaRoot(full), GtkFileChooserSchema.SchemaId)

    GtkFileChooserSchema.isInstalled(Map("XDG_DATA_DIRS" -> s"$empty$Separator$full"), home) shouldBe true
  }

  it should "read GSETTINGS_SCHEMA_DIR as a directory holding the compiled file itself" in {
    val schemas = tempDir()
    writeCompiled(schemas, GtkFileChooserSchema.SchemaId)

    GtkFileChooserSchema.isInstalled(
      Map("GSETTINGS_SCHEMA_DIR" -> schemas.toString, "XDG_DATA_DIRS" -> tempDir().toString),
      home
    ) shouldBe true
  }

  it should "read XDG_DATA_HOME ahead of the system directories" in {
    val userData = tempDir()
    writeCompiled(schemaRoot(userData), GtkFileChooserSchema.SchemaId)

    GtkFileChooserSchema.isInstalled(
      Map("XDG_DATA_HOME" -> userData.toString, "XDG_DATA_DIRS" -> tempDir().toString),
      home
    ) shouldBe true
  }

  "the schema search path" should "split GLib's search-path variables on the platform's separator, so drive letters survive" in {
    GtkFileChooserSchema
      .searchDirectories(Map("GSETTINGS_SCHEMA_DIR" -> """C:\a;D:\b""", "XDG_DATA_DIRS" -> """E:\c"""), home, ';')
      .take(2) shouldBe List(Path.of("""C:\a"""), Path.of("""D:\b"""))
  }

  it should "split on a colon where the platform separator is one" in {
    GtkFileChooserSchema
      .searchDirectories(Map("GSETTINGS_SCHEMA_DIR" -> "/a:/b"), home, ':')
      .take(2) shouldBe List(Path.of("/a"), Path.of("/b"))
  }

  it should "default XDG_DATA_HOME to ~/.local/share and XDG_DATA_DIRS to the system share dirs" in {
    GtkFileChooserSchema.searchDirectories(Map.empty, home) shouldBe List(
      home.resolve(".local/share/glib-2.0/schemas"),
      Path.of("/usr/local/share/glib-2.0/schemas"),
      Path.of("/usr/share/glib-2.0/schemas")
    )
  }

  it should "treat empty variables as unset, as the XDG specification says" in {
    GtkFileChooserSchema.searchDirectories(
      Map("XDG_DATA_HOME" -> "", "XDG_DATA_DIRS" -> ""),
      home
    ) shouldBe GtkFileChooserSchema.searchDirectories(Map.empty, home)
  }

  it should "put GSETTINGS_SCHEMA_DIR entries first, unchanged" in {
    GtkFileChooserSchema
      .searchDirectories(Map("GSETTINGS_SCHEMA_DIR" -> s"/a${Separator}/b", "XDG_DATA_DIRS" -> "/c"), home)
      .take(3) shouldBe List(Path.of("/a"), Path.of("/b"), home.resolve(".local/share/glib-2.0/schemas"))
  }
