package com.serenity.richtext

import java.nio.charset.{Charset, StandardCharsets}

import scala.annotation.tailrec
import scala.util.Try

/** The document-wide tables of an RTF file: default code page, font table, colour table and stylesheet. */
final private[richtext] case class RtfHeader(
    defaultCodePage: Int,
    fonts: Map[Int, RtfFont],
    colors: Vector[Option[String]],
    paragraphStyles: Map[Int, RtfStyle],
    characterStyles: Map[Int, RtfStyle]
):
  val resolvedParagraphStyles: Map[Int, RtfResolvedStyle] =
    paragraphStyles.keys.map(id => id -> RtfResolvedStyle.resolve(chain(paragraphStyles, id))).toMap

  val resolvedCharacterStyles: Map[Int, RtfResolvedStyle] =
    characterStyles.keys.map(id => id -> RtfResolvedStyle.resolve(chain(characterStyles, id))).toMap

  def colorAt(index: Int): Option[String] =
    colors.lift(index).flatten

  def codePageOf(font: Option[Int]): Int =
    font.flatMap(fonts.get).flatMap(_.codePage).getOrElse(defaultCodePage)

  private def chain(styles: Map[Int, RtfStyle], id: Int): List[RtfStyle] =
    collectChain(styles, Some(id), Set.empty, Nil)

  @tailrec
  private def collectChain(
    styles: Map[Int, RtfStyle],
    next: Option[Int],
    seen: Set[Int],
    collected: List[RtfStyle]
  ): List[RtfStyle] =
    next.filterNot(seen.contains).flatMap(id => styles.get(id)) match
      case Some(style) => collectChain(styles, style.basedOn, seen + style.id, style :: collected)
      case None        => collected.reverse

private[richtext] object RtfHeader:
  private val FontTableWord       = "fonttbl"
  private val ColorTableWord      = "colortbl"
  private val StylesheetWord      = "stylesheet"
  private val AnsiCodePage        = 1252
  private val MacCodePage         = 10000
  private val OemCodePage         = 437
  private val OemMultilingualPage = 850

  /** Windows character-set ids from `\fcharset`, for the sets whose bytes differ from the document code page. */
  private val CodePageByCharset: Map[Int, Int] = Map(
    77  -> 10000,
    128 -> 932,
    129 -> 949,
    130 -> 1361,
    134 -> 936,
    136 -> 950,
    161 -> 1253,
    162 -> 1254,
    163 -> 1258,
    177 -> 1255,
    178 -> 1256,
    186 -> 1257,
    204 -> 1251,
    222 -> 874,
    238 -> 1250,
    254 -> 437,
    255 -> 850
  )

  val empty: RtfHeader = RtfHeader(AnsiCodePage, Map.empty, Vector.empty, Map.empty, Map.empty)

  def read(root: RtfNode.Group): RtfHeader =
    val controls = root.children.collect { case RtfNode.Control(name, parameter) => name -> parameter }
    val tables   = root.children.collect { case group: RtfNode.Group => group }
    val styles   = tables.filter(destinationOf(_).contains(StylesheetWord)).flatMap(styleEntries).flatMap(styleOf)
    RtfHeader(
      defaultCodePage = defaultCodePage(controls),
      fonts = tables.filter(destinationOf(_).contains(FontTableWord)).flatMap(entriesOf).flatMap(fontOf).toMap,
      colors = tables.filter(destinationOf(_).contains(ColorTableWord)).flatMap(colorEntries).toVector,
      paragraphStyles = styles.filter(_.kind == RtfStyleKind.Paragraph).map(style => style.id -> style).toMap,
      characterStyles = styles.filter(_.kind == RtfStyleKind.Character).map(style => style.id -> style).toMap
    )

  /** The name of the control word that opens a group, looking past the `\*` ignorable-destination marker. */
  def destinationOf(group: RtfNode.Group): Option[String] =
    group.children.dropWhile(isIgnorableMarker).headOption.collect { case RtfNode.Control(name, _) => name }

  def isIgnorable(group: RtfNode.Group): Boolean =
    group.children.headOption.exists(isIgnorableMarker)

  private def isIgnorableMarker(node: RtfNode): Boolean =
    node match
      case RtfNode.Control("*", _) => true
      case _                       => false

  def charsetFor(codePage: Int): Charset =
    val candidates = List(s"windows-$codePage", s"x-windows-$codePage", s"IBM$codePage", s"MS$codePage", s"cp$codePage")
    candidates
      .flatMap(name => Try(Charset.forName(name)).toOption)
      .headOption
      .orElse(Option.when(codePage == MacCodePage)(Charset.forName("x-MacRoman")))
      .orElse(Option.when(codePage == 65001)(StandardCharsets.UTF_8))
      .getOrElse(StandardCharsets.ISO_8859_1)

  private def defaultCodePage(controls: Vector[(String, Option[Int])]): Int =
    controls
      .collectFirst { case ("ansicpg", Some(codePage)) => codePage }
      .orElse(controls.collectFirst { case ("mac", _) => MacCodePage })
      .orElse(controls.collectFirst { case ("pca", _) => OemMultilingualPage })
      .orElse(controls.collectFirst { case ("pc", _) => OemCodePage })
      .getOrElse(AnsiCodePage)

  /** Entries of a table: each nested group is one entry, and bare nodes are split into entries at every semicolon. */
  private def entriesOf(table: RtfNode.Group): Vector[Vector[RtfNode]] =
    val body = bodyOf(table)
    body.collect { case group: RtfNode.Group => group.children } ++
      splitAtSemicolons(body.filterNot(isGroup))

  private def bodyOf(table: RtfNode.Group): Vector[RtfNode] =
    table.children.dropWhile(isIgnorableMarker).drop(1)

  private def isGroup(node: RtfNode): Boolean =
    node match
      case _: RtfNode.Group => true
      case _                => false

  private def splitAtSemicolons(nodes: Vector[RtfNode]): Vector[Vector[RtfNode]] =
    val (finished, current) = nodes.foldLeft((Vector.empty[Vector[RtfNode]], Vector.empty[RtfNode])) {
      case ((entries, open), RtfNode.Text(value)) if value.contains(';') =>
        val parts = value.split(";", -1).toVector
        val closed = parts.init.foldLeft((entries, open)) {
          case ((done, partial), part) =>
            (done :+ (partial ++ textNode(part)), Vector.empty[RtfNode])
        }
        (closed._1, closed._2 ++ textNode(parts.lastOption.getOrElse("")))
      case ((entries, open), node) => (entries, open :+ node)
    }
    if current.isEmpty then finished else finished :+ current

  private def textNode(value: String): Vector[RtfNode] =
    if value.isEmpty then Vector.empty else Vector(RtfNode.Text(value))

  private def entryName(entry: Vector[RtfNode]): String =
    entry.collect { case RtfNode.Text(value) => value }.mkString.takeWhile(_ != ';').trim

  private def controlsOf(entry: Vector[RtfNode]): Vector[(String, Option[Int])] =
    entry.collect { case RtfNode.Control(name, parameter) => name -> parameter }

  private def fontOf(entry: Vector[RtfNode]): Option[(Int, RtfFont)] =
    val controls = controlsOf(entry)
    controls.collectFirst { case ("f", Some(index)) => index }.map { index =>
      val charset  = controls.collectFirst { case ("fcharset", Some(id)) => id }.flatMap(CodePageByCharset.get)
      val codePage = controls.collectFirst { case ("cpg", Some(page)) => page }.orElse(charset)
      index -> RtfFont(entryName(entry), codePage)
    }

  private def colorEntries(table: RtfNode.Group): Vector[Option[String]] =
    splitAtSemicolons(bodyOf(table).filterNot(isGroup)).map { entry =>
      val controls = controlsOf(entry).toMap
      Option.when(Set("red", "green", "blue").exists(controls.contains)) {
        val red   = channel(controls, "red")
        val green = channel(controls, "green")
        val blue  = channel(controls, "blue")
        f"#$red%02x$green%02x$blue%02x"
      }
    }

  private def channel(controls: Map[String, Option[Int]], name: String): Int =
    controls.get(name).flatten.getOrElse(0).max(0).min(255)

  private def styleEntries(table: RtfNode.Group): Vector[Vector[RtfNode]] =
    entriesOf(table)

  private def styleOf(entry: Vector[RtfNode]): Option[RtfStyle] =
    val controls = controlsOf(entry)
    val kind = controls.collectFirst {
      case ("cs", Some(id)) => (RtfStyleKind.Character, id)
      case ("s", Some(id))  => (RtfStyleKind.Paragraph, id)
    }
    val isOtherKind = controls.exists((name, _) => name == "ds" || name == "ts")
    Option.unless(isOtherKind && kind.isEmpty) {
      val (styleKind, id) = kind.getOrElse((RtfStyleKind.Paragraph, 0))
      RtfStyle(
        kind = styleKind,
        id = id,
        name = entryName(entry),
        basedOn = controls.collectFirst { case ("sbasedon", Some(base)) => base },
        char = controls.foldLeft(RtfCharFormat.empty)((format, control) =>
          format.withControl(control._1, control._2).getOrElse(format)
        ),
        alignment = controls.flatMap((name, _) => alignmentOf(name)).lastOption,
        outlineLevel = controls.collectFirst { case ("outlinelevel", Some(level)) => level },
        dropCapLines = controls.collectFirst { case ("dropcapli", Some(lines)) if lines > 0 => lines }
      )
    }

  def alignmentOf(word: String): Option[ParagraphAlignment] =
    word match
      case "ql" => Some(ParagraphAlignment.Left)
      case "qc" => Some(ParagraphAlignment.Center)
      case "qr" => Some(ParagraphAlignment.Right)
      case "qj" => Some(ParagraphAlignment.Justify)
      case _    => None
