package com.serenity.richtext

/** Character formatting as RTF states it: every field is `None` until a control word sets it, so layers (paragraph
  * style, character style, direct formatting) can be overlaid with later layers winning.
  */
final private[richtext] case class RtfCharFormat(
    bold: Option[Boolean] = None,
    italic: Option[Boolean] = None,
    underline: Option[Boolean] = None,
    font: Option[Int] = None,
    halfPoints: Option[Int] = None,
    color: Option[Int] = None
):

  def overlay(top: RtfCharFormat): RtfCharFormat =
    RtfCharFormat(
      bold = top.bold.orElse(bold),
      italic = top.italic.orElse(italic),
      underline = top.underline.orElse(underline),
      font = top.font.orElse(font),
      halfPoints = top.halfPoints.orElse(halfPoints),
      color = top.color.orElse(color)
    )

  /** The format after applying one control word, or `None` when the word does not carry character formatting. */
  def withControl(name: String, parameter: Option[Int]): Option[RtfCharFormat] =
    name match
      case "b"                                                 => Some(copy(bold = Some(isOn(parameter))))
      case "i"                                                 => Some(copy(italic = Some(isOn(parameter))))
      case "ulnone"                                            => Some(copy(underline = Some(false)))
      case word if RtfCharFormat.UnderlineWords.contains(word) => Some(copy(underline = Some(isOn(parameter))))
      case "f"                                                 => parameter.map(index => copy(font = Some(index)))
      case "fs"                                                => parameter.map(size => copy(halfPoints = Some(size)))
      case "cf"                                                => parameter.map(index => copy(color = Some(index)))
      case _                                                   => None

  private def isOn(parameter: Option[Int]): Boolean =
    parameter.forall(_ != 0)

private[richtext] object RtfCharFormat:
  val empty: RtfCharFormat = RtfCharFormat()

  private val UnderlineWords: Set[String] = Set(
    "ul",
    "uld",
    "uldb",
    "ulw",
    "ulth",
    "ulthd",
    "ulthdash",
    "ulwave",
    "uldash",
    "uldashd",
    "uldashdd",
    "ulhwave",
    "ulldash",
    "ulthldash",
    "ululdbwave"
  )

  /** Character formatting Serenity's model has no field for, keyed by the name reported through fidelity. */
  val Unmodelled: Map[String, String] = Map(
    "strike"    -> "strikethrough",
    "striked"   -> "strikethrough",
    "super"     -> "superscript",
    "sub"       -> "subscript",
    "highlight" -> "highlight",
    "scaps"     -> "small capitals"
  )

private[richtext] enum RtfStyleKind:
  case Paragraph
  case Character

final private[richtext] case class RtfFont(name: String, codePage: Option[Int])

final private[richtext] case class RtfStyle(
    kind: RtfStyleKind,
    id: Int,
    name: String,
    basedOn: Option[Int],
    char: RtfCharFormat,
    alignment: Option[ParagraphAlignment],
    outlineLevel: Option[Int],
    dropCapLines: Option[Int]
)

/** A paragraph style flattened along its `\sbasedon` chain, nearest definition winning. */
final private[richtext] case class RtfResolvedStyle(
    char: RtfCharFormat,
    alignment: Option[ParagraphAlignment],
    outlineLevel: Option[Int],
    headingLevelByName: Option[Int],
    dropCapLines: Option[Int]
)

private[richtext] object RtfResolvedStyle:
  val empty: RtfResolvedStyle = RtfResolvedStyle(RtfCharFormat.empty, None, None, None, None)

  private val HeadingName = """(?i)\s*heading\s*(\d+)\s*""".r

  def resolve(chain: List[RtfStyle]): RtfResolvedStyle =
    RtfResolvedStyle(
      char = chain.reverse.foldLeft(RtfCharFormat.empty)((below, style) => below.overlay(style.char)),
      alignment = chain.flatMap(_.alignment).headOption,
      outlineLevel = chain.flatMap(_.outlineLevel).headOption,
      headingLevelByName = chain.flatMap(style => headingLevel(style.name)).headOption,
      dropCapLines = chain.flatMap(_.dropCapLines).headOption
    )

  private def headingLevel(name: String): Option[Int] =
    name match
      case HeadingName(digits) => digits.toIntOption.map(_.max(1))
      case _                   => None
