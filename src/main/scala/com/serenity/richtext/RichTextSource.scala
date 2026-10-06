package com.serenity.richtext

import java.nio.charset.{Charset, StandardCharsets}

/** An XML property kept as the source wrote it because the model cannot say all of it. `modelled` is what the model
  * made of the element when it was imported, so the writer can tell whether the model's value has since changed: if
  * not, `raw` goes back out; if so, the model's value wins and `raw` is dropped.
  */
final case class RawProperty(name: String, raw: String, modelled: Option[String] = None)

object RawProperty:
  /** Names starting with `@` are not source properties but notes the codecs keep on a run, such as the attributes of a
    * link element; writers never emit them as properties.
    */
  val LinkAttributes: String = "@link-attributes"

  def isNote(property: RawProperty): Boolean = property.name.startsWith("@")

  /** The attributes of a link element other than the ones that carry its target, kept as `link`'s notes. */
  def linkAttributes(raw: String, link: Option[String]): List[RawProperty] =
    Option.when(raw.trim.nonEmpty)(RawProperty(LinkAttributes, raw, link)).toList

  /** The attribute text a link element had besides its target, if still for `link`. */
  def linkAttributesFor(style: RichTextStyle): String =
    style.extras.find(extra => extra.name == LinkAttributes && extra.modelled == style.link).fold("")(_.raw)

/** What a paragraph decoded from a package remembers about where it came from.
  *
  * `blockIndex` is the body block it came from (or [[ParagraphSource.NoBlock]] for a paragraph derived from one, which
  * remembers that block as `derivedFrom`). `imported` is what the model made of it at import: a paragraph still equal
  * to it is untouched and is copied from the source bytes. `openTagAttributes` and `properties` are the unmodelled
  * parts a touched paragraph is rewritten with.
  */
final case class ParagraphSource(
    blockIndex: Int,
    openTagAttributes: String,
    properties: List[RawProperty],
    imported: Option[RichTextParagraph],
    derivedFrom: Option[Int] = None
):

  /** The block this paragraph's formatting came from, whether it is that block's paragraph or was derived from it. */
  def originBlock: Option[Int] =
    Option.when(blockIndex != ParagraphSource.NoBlock)(blockIndex).orElse(derivedFrom)

  /** The same formatting, minus the claim to be the imported paragraph and the identifiers that must stay unique. */
  def asDerived: ParagraphSource =
    copy(
      blockIndex = ParagraphSource.NoBlock,
      openTagAttributes = ParagraphSource.UniqueIdentifiers.replaceAllIn(openTagAttributes, ""),
      imported = None,
      derivedFrom = originBlock
    )

object ParagraphSource:
  val NoBlock: Int = -1

  private val UniqueIdentifiers = """\s+w14:(paraId|textId)="[^"]*"""".r

/** What a child of the document body becomes in the model. */
enum BodyKind:
  /** A paragraph, modelled. */
  case Paragraph

  /** A construct the model does not hold, shown as a read-only line and written back as it was. */
  case Block(feature: DocumentFeature)

  /** Markup with no content of its own (section properties, range markers): kept in place and never shown. */
  case Structural

/** One child of the document body, verbatim, with the whitespace that followed it. `elementLength` is where the element
  * ends inside `markup`.
  */
final case class BodyBlock(markup: String, elementLength: Int, kind: BodyKind):
  def element: String = markup.take(elementLength)
  def gap: String     = markup.drop(elementLength)

  /** Whether the model has a paragraph for this child, which then decides what is written in its place. */
  def isModelled: Boolean = kind != BodyKind.Structural

/** The main XML part cut at its body children: `head` up to the first, `tail` from the body's end tag. */
final case class BodySource(head: String, blocks: Vector[BodyBlock], tail: String)

/** The text of the main XML part, with what it takes to encode it back to the same bytes. */
final case class MainPart(text: String, charset: Charset, byteOrderMark: Boolean):

  def encode(content: String): Array[Byte] =
    val bytes = content.getBytes(charset)
    if byteOrderMark then MainPart.Utf8Bom ++ bytes else bytes

object MainPart:
  private val Utf8Bom: Array[Byte] = Array(0xef.toByte, 0xbb.toByte, 0xbf.toByte)

  /** The part as text, or `None` when decoding and encoding again would not give back the same bytes. */
  def decode(bytes: Array[Byte]): Option[MainPart] =
    val hasBom  = bytes.length >= 3 && bytes.take(3).sameElements(Utf8Bom)
    val payload = if hasBom then bytes.drop(3) else bytes
    val part    = MainPart(String(payload, StandardCharsets.UTF_8), StandardCharsets.UTF_8, hasBom)
    Option.when(part.encode(part.text).sameElements(bytes))(part)

enum PackageFormat:
  case Docx
  case Odt

/** The package a document was read from, kept so that a save writes back everything the model does not own. `main` and
  * `body` are absent when the main part could not be cut into byte-exact slices; every other part still passes through.
  */
final case class DocumentSource(
    format: PackageFormat,
    archive: Array[Byte],
    mainEntry: String,
    main: Option[MainPart],
    body: Option[BodySource],
    relationships: Option[String],
    entryNames: Set[String] = Set.empty
)
