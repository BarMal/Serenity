package com.serenity.io

import java.nio.file.Path

import com.serenity.lsp.config.{FileExtension, LanguageId}
import com.serenity.richtext.SaveTarget

enum FileType:
  case Scala
  case Java
  case JavaScript
  case TypeScript
  case Python
  case Rust
  case Go
  case C
  case Cpp
  case Kotlin
  case CSharp
  case Ruby
  case Haskell
  case Lua
  case Html
  case Css
  case Markdown
  case Text
  case WordDocument
  case WordOpenXmlDocument
  case OpenDocumentText
  case RichText
  case Json
  case Xml
  case Yaml
  case Toml
  case Config
  case Sql
  case Shell
  case Unknown

  def displayName: String =
    FileType.displayName(this)

object FileType:

  def fromPath(path: Path): FileType =
    val fileName = path.getFileName.toString.toLowerCase
    fileName.lastIndexOf('.') match
      case -1 => FileType.Unknown
      case dotIndex =>
        val extension = fileName.substring(dotIndex + 1)
        fromExtension(extension)

  /** Source and markup files take their type from the language table the editor and LSP use, so the two cannot classify
    * one extension differently (#1897). Only formats that are not languages are listed here.
    */
  def fromExtension(extension: String): FileType =
    FileExtension.languageIdFor(extension).fold(documentType(extension))(fromLanguage)

  private def documentType(extension: String): FileType =
    extension.toLowerCase match
      case "txt"             => FileType.Text
      case "doc"             => FileType.WordDocument
      case "docx"            => FileType.WordOpenXmlDocument
      case "odt"             => FileType.OpenDocumentText
      case "rtf"             => FileType.RichText
      case "conf" | "config" => FileType.Config
      case "sh" | "bash"     => FileType.Shell
      case _                 => FileType.Unknown

  def fromLanguage(language: LanguageId): FileType =
    language match
      case LanguageId.Scala      => FileType.Scala
      case LanguageId.Java       => FileType.Java
      case LanguageId.JavaScript => FileType.JavaScript
      case LanguageId.TypeScript => FileType.TypeScript
      case LanguageId.Python     => FileType.Python
      case LanguageId.Rust       => FileType.Rust
      case LanguageId.Go         => FileType.Go
      case LanguageId.C          => FileType.C
      case LanguageId.Cpp        => FileType.Cpp
      case LanguageId.Kotlin     => FileType.Kotlin
      case LanguageId.CSharp     => FileType.CSharp
      case LanguageId.Ruby       => FileType.Ruby
      case LanguageId.Haskell    => FileType.Haskell
      case LanguageId.Lua        => FileType.Lua
      case LanguageId.Html       => FileType.Html
      case LanguageId.Css        => FileType.Css
      case LanguageId.Markdown   => FileType.Markdown
      case LanguageId.JsonLang   => FileType.Json
      case LanguageId.Xml        => FileType.Xml
      case LanguageId.Yaml       => FileType.Yaml
      case LanguageId.Toml       => FileType.Toml
      case LanguageId.Sql        => FileType.Sql

  /** The format a rich document is written in when saved as `fileType`; every other type is saved as plain text. */
  def saveTarget(fileType: FileType): SaveTarget =
    fileType match
      case FileType.WordOpenXmlDocument => SaveTarget.Docx
      case FileType.OpenDocumentText    => SaveTarget.Odt
      case FileType.RichText            => SaveTarget.Rtf
      case FileType.Markdown            => SaveTarget.Markdown
      case _                            => SaveTarget.PlainText

  def displayName(fileType: FileType): String = fileType match
    case FileType.Scala      => "Scala"
    case FileType.Java       => "Java"
    case FileType.JavaScript => "JavaScript"
    case FileType.TypeScript => "TypeScript"
    case FileType.Python     => "Python"
    case FileType.Rust       => "Rust"
    case FileType.Go         => "Go"
    case FileType.C          => "C"
    case FileType.Cpp        => "C++"
    case FileType.Kotlin     => "Kotlin"
    case FileType.CSharp     => "C#"
    case FileType.Ruby       => "Ruby"
    case FileType.Haskell    => "Haskell"
    case FileType.Lua        => "Lua"
    case FileType.Html       => "HTML"
    case FileType.Css        => "CSS"
    case FileType.Markdown   => "Markdown"
    case FileType.Text       => "Text"
    case FileType.WordDocument =>
      "Legacy Word Document (.doc, unsupported)"
    case FileType.WordOpenXmlDocument =>
      "Word Open XML Document"
    case FileType.OpenDocumentText =>
      "OpenDocument Text"
    case FileType.RichText =>
      "Rich Text"
    case FileType.Json    => "JSON"
    case FileType.Xml     => "XML"
    case FileType.Yaml    => "YAML"
    case FileType.Toml    => "TOML"
    case FileType.Config  => "Config"
    case FileType.Sql     => "SQL"
    case FileType.Shell   => "Shell"
    case FileType.Unknown => "Unknown"

/** The formats `FileManager.saveBuffer` handles as genuinely distinct write paths -- a narrower, cyclable subset of
  * [[FileType]] for the Save As dialog's format field.
  */
enum SaveFormat:
  case Text
  case Markdown
  case RichText
  case OpenDocumentText
  case WordOpenXml

  def canonicalExtension: String = SaveFormat.canonicalExtension(this)
  def displayName: String        = SaveFormat.displayName(this)

object SaveFormat:

  /** Cycling order for the Save As dialog's format field. */
  val ordered: List[SaveFormat] = List(Text, Markdown, RichText, OpenDocumentText, WordOpenXml)

  def canonicalExtension(format: SaveFormat): String = format match
    case SaveFormat.Text             => ".txt"
    case SaveFormat.Markdown         => ".md"
    case SaveFormat.RichText         => ".rtf"
    case SaveFormat.OpenDocumentText => ".odt"
    case SaveFormat.WordOpenXml      => ".docx"

  def displayName(format: SaveFormat): String = format match
    case SaveFormat.Text             => "Text"
    case SaveFormat.Markdown         => "Markdown"
    case SaveFormat.RichText         => "Rich Text"
    case SaveFormat.OpenDocumentText => "OpenDocument Text"
    case SaveFormat.WordOpenXml      => "Word"

  /** Maps a detected [[FileType]] back to the matching [[SaveFormat]]. `FileType.Text` and every format outside the
    * cyclable five (e.g. `FileType.Scala`) fall back to `SaveFormat.Text`, matching
    * `FileWorkflowState.detectedFileType`'s own default-to-Text behaviour for an unrecognized or absent extension.
    */
  def fromFileType(fileType: FileType): SaveFormat = fileType match
    case FileType.Markdown            => SaveFormat.Markdown
    case FileType.RichText            => SaveFormat.RichText
    case FileType.OpenDocumentText    => SaveFormat.OpenDocumentText
    case FileType.WordOpenXmlDocument => SaveFormat.WordOpenXml
    case _                            => SaveFormat.Text

enum DocumentFormat:
  case PlainText
  case Markdown
  case RichTextDocument
  case SourceCode
  case StructuredText
  case Unknown

final case class DocumentFormatCapabilities(
    canOpen: Boolean,
    canSave: Boolean,
    canRender: Boolean,
    canEdit: Boolean,
    preservesRichFormatting: Boolean
)

object DocumentFormat:

  def fromFileType(fileType: FileType): DocumentFormat =
    fileType match
      case FileType.Text =>
        DocumentFormat.PlainText
      case FileType.Markdown =>
        DocumentFormat.Markdown
      case FileType.WordDocument | FileType.WordOpenXmlDocument | FileType.OpenDocumentText | FileType.RichText =>
        DocumentFormat.RichTextDocument
      case FileType.Json | FileType.Xml | FileType.Yaml | FileType.Toml | FileType.Config | FileType.Sql =>
        DocumentFormat.StructuredText
      case FileType.Unknown =>
        DocumentFormat.Unknown
      case _ =>
        DocumentFormat.SourceCode

  def fromPath(path: Path): DocumentFormat =
    fromFileType(FileType.fromPath(path))

  def capabilities(format: DocumentFormat): DocumentFormatCapabilities =
    format match
      case DocumentFormat.PlainText =>
        DocumentFormatCapabilities(
          canOpen = true,
          canSave = true,
          canRender = true,
          canEdit = true,
          preservesRichFormatting = false
        )
      case DocumentFormat.Markdown =>
        DocumentFormatCapabilities(
          canOpen = true,
          canSave = true,
          canRender = true,
          canEdit = true,
          preservesRichFormatting = false
        )
      case DocumentFormat.SourceCode | DocumentFormat.StructuredText =>
        DocumentFormatCapabilities(
          canOpen = true,
          canSave = true,
          canRender = true,
          canEdit = true,
          preservesRichFormatting = false
        )
      case DocumentFormat.Unknown =>
        DocumentFormatCapabilities(
          canOpen = true,
          canSave = true,
          canRender = true,
          canEdit = true,
          preservesRichFormatting = false
        )
      case DocumentFormat.RichTextDocument =>
        DocumentFormatCapabilities(
          canOpen = true,
          canSave = true,
          canRender = true,
          canEdit = true,
          preservesRichFormatting = true
        )

  /** Whether saving a buffer that currently carries rich formatting at `targetFileType` would discard it -- the
    * proactive counterpart to `LossyRichTextOverwriteException` (issue #1253): that exception fires only after a failed
    * save, and only for a lossy *re-import*, whereas this covers any save whose target format can't represent
    * marks/alignment/roles at all.
    */
  def wouldLoseFormatting(hasRichFormatting: Boolean, targetFileType: FileType): Boolean =
    hasRichFormatting && !capabilities(targetFileType).preservesRichFormatting

  def capabilities(fileType: FileType): DocumentFormatCapabilities =
    fileType match
      case FileType.WordDocument =>
        DocumentFormatCapabilities(
          canOpen = false,
          canSave = false,
          canRender = false,
          canEdit = false,
          preservesRichFormatting = false
        )
      case FileType.RichText | FileType.OpenDocumentText | FileType.WordOpenXmlDocument =>
        // All three round-trip marks, alignment and headings through their own codecs, so all three genuinely
        // preserve rich formatting. This previously reported only RTF as lossless and both ODT and WordOpenXml as
        // lossy -- backwards from the codecs' actual behaviour (#1291).
        DocumentFormatCapabilities(
          canOpen = true,
          canSave = true,
          canRender = true,
          canEdit = true,
          preservesRichFormatting = true
        )
      case _ =>
        capabilities(fromFileType(fileType))
