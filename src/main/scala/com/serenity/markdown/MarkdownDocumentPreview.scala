package com.serenity.markdown

object MarkdownDocumentPreview:

  def render(source: String, maxWidth: Int): List[String] =
    val width = maxWidth.max(12)
    renderLines(source.linesIterator.toVector, width)

  def renderInlineLine(source: String): String =
    val trimmed = source.trim
    val Heading = """^(#{1,6})\s+(.+)$""".r
    trimmed match
      case Heading(_, text) =>
        normalizeInline(text).trim
      case text if text.startsWith(">") =>
        s"| ${normalizeInline(text.drop(1)).trim}"
      case text =>
        normalizeInline(text)

  private def renderLines(lines: Vector[String], maxWidth: Int): List[String] =
    @annotation.tailrec
    def loop(index: Int, inFence: Boolean, acc: List[String]): List[String] =
      if index >= lines.length then acc.reverse
      else
        val line    = lines(index)
        val trimmed = line.trim

        if isFenceLine(trimmed) then
          val language = trimmed.dropWhile(ch => ch == '`' || ch == '~').trim
          val row      = Option.when(!inFence && language.nonEmpty)(s"Code: $language")
          loop(index + 1, !inFence, row.toList.reverse ::: acc)
        else if inFence then loop(index + 1, inFence, s"    $line" :: acc)
        else if trimmed.isEmpty then loop(index + 1, inFence, "" :: acc)
        else if isTableLine(trimmed) then
          val tableLines = lines.drop(index).takeWhile(line => isTableLine(line.trim)).toList
          loop(index + tableLines.length, inFence, renderTable(tableLines).reverse ::: acc)
        else
          val rendered = renderSingleLine(trimmed, maxWidth)
          loop(index + 1, inFence, rendered.reverse ::: acc)

    loop(0, inFence = false, Nil)

  private def renderSingleLine(line: String, maxWidth: Int): List[String] =
    heading(line)
      .orElse(imageLine(line))
      .getOrElse {
        val normalized = normalizeInline(line) match
          case text if text.startsWith(">") =>
            s"| ${text.drop(1).trim}"
          case text => text
        wrap(normalized, maxWidth)
      }

  private def heading(line: String): Option[List[String]] =
    val Heading = """^(#{1,6})\s+(.+)$""".r
    line match
      case Heading(markers, text) =>
        val normalized = normalizeInline(text).trim
        val rendered =
          markers.length match
            case 1 =>
              val title = normalized.toUpperCase
              List(title, "=" * title.length)
            case 2 =>
              List(normalized, "-" * normalized.length)
            case level =>
              List(s"${"#" * level} $normalized")
        Some(rendered)
      case _ => None

  private def imageLine(line: String): Option[List[String]] =
    val Image = """^!\[([^\]]*)\]\(([^)]+)\)$""".r
    line match
      case Image(alt, target) =>
        val label = Option(alt.trim).filter(_.nonEmpty).getOrElse("Image")
        Some(List(s"Image: $label ($target)"))
      case _ => None

  private def normalizeInline(text: String): String =
    val withoutImages = """!\[([^\]]*)\]\(([^)]+)\)""".r.replaceAllIn(
      text,
      matched =>
        val label = Option(matched.group(1).trim).filter(_.nonEmpty).getOrElse("Image")
        s"Image: $label (${matched.group(2)})"
    )
    val withoutLinks = """\[([^\]]+)\]\(([^)]+)\)""".r.replaceAllIn(
      withoutImages,
      matched => s"${matched.group(1)} (${matched.group(2)})"
    )
    "`([^`]+)`".r.replaceAllIn(withoutLinks, matched => matched.group(1))

  private def renderTable(lines: List[String]): List[String] =
    val rows = lines.map(tableCells)
    rows.headOption match
      case None => Nil
      case Some(header) =>
        val dataRows = rows.tail.filterNot(isSeparatorRow)
        val widths = (header :: dataRows).foldLeft(header.map(_.length)) { (current, row) =>
          current.zipAll(row.map(_.length), 0, 0).map(_ max _)
        }
        val renderedHeader    = renderTableRow(header, widths)
        val renderedSeparator = widths.map(width => "-" * width.max(1)).mkString("  ")
        renderedHeader :: renderedSeparator :: dataRows.map(row => renderTableRow(row, widths))

  private def tableCells(line: String): List[String] =
    line.trim
      .stripPrefix("|")
      .stripSuffix("|")
      .split("\\|")
      .toList
      .map(_.trim)

  private def isSeparatorRow(cells: List[String]): Boolean =
    cells.nonEmpty && cells.forall(_.matches(""":?-{3,}:?"""))

  private def renderTableRow(cells: List[String], widths: List[Int]): String =
    widths.zipWithIndex
      .map {
        case (width, index) =>
          cells.lift(index).getOrElse("").padTo(width, ' ')
      }
      .mkString("  ")
      .stripTrailing()

  private def wrap(text: String, maxWidth: Int): List[String] =
    val words = text.split("\\s+").toList.filter(_.nonEmpty)
    if words.isEmpty then List("")
    else
      words
        .foldLeft(List.empty[String]) { (lines, word) =>
          lines match
            case Nil => List(word)
            case init :+ last if last.length + 1 + word.length <= maxWidth =>
              init :+ s"$last $word"
            case _ =>
              lines :+ word
        }

  private def isFenceLine(trimmed: String): Boolean =
    trimmed.startsWith("```") || trimmed.startsWith("~~~")

  private def isTableLine(trimmed: String): Boolean =
    trimmed.contains("|") && trimmed.count(_ == '|') >= 2

end MarkdownDocumentPreview
