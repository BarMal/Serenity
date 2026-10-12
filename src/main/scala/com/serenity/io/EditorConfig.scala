package com.serenity.io

import java.io.File
import java.nio.file.Path
import java.util.Locale

import com.serenity.state.models.IndentationHint

/** One section of an `.editorconfig` file: the properties it sets for the paths its name matches, in file order. */
final case class EditorConfigSection(glob: EditorConfigGlob, properties: Vector[(String, String)])

/** The parsed text of one `.editorconfig` file. */
final case class EditorConfigFile(root: Boolean, sections: Vector[EditorConfigSection])

/** `.editorconfig` files (https://spec.editorconfig.org): parsing one, settling the properties that apply to a path
  * from the files on the way up to it, and reading the indentation keys among them.
  */
object EditorConfig:

  /** A file found in `directory`, whose section names are relative to it. */
  final case class Layer(directory: Path, file: EditorConfigFile)

  val FileName: String = ".editorconfig"

  private val Property = """([^:=\s][^:=]*?)\s*[:=]\s*(.*)""".r

  def parse(text: String): EditorConfigFile =
    val parsed = text.stripPrefix("﻿").linesIterator.map(_.trim).foldLeft(Parsing.Start)(_.reading(_))
    EditorConfigFile(parsed.root, parsed.sections)

  final private case class Parsing(root: Boolean, sections: Vector[EditorConfigSection]):

    def reading(line: String): Parsing =
      if line.isEmpty || line.startsWith("#") || line.startsWith(";") then this
      else if line.startsWith("[") && line.endsWith("]") then
        val glob = EditorConfigGlob.compile(line.drop(1).dropRight(1))
        copy(sections = sections :+ EditorConfigSection(glob, Vector.empty))
      else
        line match
          case Property(key, value) => withProperty(key.toLowerCase(Locale.ROOT), value.trim.toLowerCase(Locale.ROOT))
          case _                    => this

    // Before the first section only `root` means anything.
    private def withProperty(key: String, value: String): Parsing =
      sections.lastOption match
        case None => if key == "root" then copy(root = value == "true") else this
        case Some(section) =>
          val extended = section.copy(properties = section.properties :+ (key -> value))
          copy(sections = sections.dropRight(1) :+ extended)

  private object Parsing:
    val Start: Parsing = Parsing(root = false, Vector.empty)

  private val Unset = "unset"

  /** The properties that apply to `path`, given the files from the farthest directory to the nearest: a later section
    * overrides an earlier one, and `unset` takes a property away again.
    */
  def propertiesFor(layers: List[Layer], path: Path): Map[String, String] =
    layers.foldLeft(Map.empty[String, String]) { (settled, layer) =>
      val relative = separatedByForwardSlashes(layer.directory.relativize(path))
      layer.file.sections
        .filter(_.glob.matches(relative))
        .flatMap(_.properties)
        .foldLeft(settled) {
          case (properties, (key, Unset)) => properties - key
          case (properties, (key, value)) => properties.updated(key, value)
        }
    }

  private def separatedByForwardSlashes(path: Path): String =
    path.toString.replace(File.separatorChar, '/')

  /** What `indent_style`, `indent_size` and `tab_width` say. As the specification has it, `indent_size = tab` (and an
    * unset `indent_size` under `indent_style = tab`) means `tab_width`, and `tab_width` defaults to a numeric
    * `indent_size`. Serenity has one width for both the indent step and the tab stops, so tabs take the tab width and
    * spaces take the indent size.
    */
  def indentation(properties: Map[String, String]): IndentationHint =
    val insertSpaces = properties.get("indent_style").collect {
      case "space" => true
      case "tab"   => false
    }
    val indentSize = properties.get("indent_size").flatMap(positive)
    val tabWidth   = properties.get("tab_width").flatMap(positive).orElse(indentSize)
    val tabSize =
      if insertSpaces.contains(false) then tabWidth
      else indentSize.orElse(tabWidth)
    IndentationHint(insertSpaces, tabSize)

  private def positive(value: String): Option[Int] = value.toIntOption.filter(_ > 0)
end EditorConfig
