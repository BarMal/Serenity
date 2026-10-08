package com.serenity.richtext

/** Formatting in force at a point in the document; a group restores the state it started with when it closes. */
final private[richtext] case class RtfState(
    char: RtfCharFormat,
    characterStyle: Option[Int],
    paragraphStyle: Int,
    alignment: Option[ParagraphAlignment],
    outlineLevel: Option[Int],
    dropCapLines: Option[Int],
    unicodeSkip: Int
):
  def resetParagraph: RtfState =
    copy(paragraphStyle = 0, alignment = None, outlineLevel = None, dropCapLines = None)

  def resetCharacter: RtfState =
    copy(char = RtfCharFormat.empty, characterStyle = None)

private[richtext] object RtfState:
  val initial: RtfState = RtfState(RtfCharFormat.empty, None, 0, None, None, None, unicodeSkip = 1)

/** What has been read so far. `skip` counts the fallback characters still to discard after a `\uN` escape, and
  * `pendingBytes` collects consecutive `\'hh` escapes so multi-byte code pages decode as a unit.
  */
final private[richtext] case class RtfProgress(
    paragraphs: Vector[RichTextParagraph],
    runs: Vector[RichTextRun],
    pendingBytes: Vector[Byte],
    pendingCodePage: Int,
    pendingStyle: RichTextStyle,
    skip: Int,
    unsupported: Set[String]
):
  def report(feature: String): RtfProgress = copy(unsupported = unsupported + feature)

private[richtext] object RtfProgress:
  def initial(codePage: Int): RtfProgress =
    RtfProgress(Vector.empty, Vector.empty, Vector.empty, codePage, RichTextStyle.empty, 0, Set.empty)

/** Interprets a parsed RTF tree as a [[RichTextDocument]], reporting what the model cannot hold. */
private[richtext] object RtfReader:

  /** Destinations whose content is skipped without being a loss: document tables already read, and metadata. */
  private val SilentDestinations: Set[String] = Set(
    "fonttbl",
    "colortbl",
    "stylesheet",
    "info",
    "generator",
    "xmlnstbl",
    "rsidtbl",
    "themedata",
    "colorschememapping",
    "latentstyles",
    "datastore",
    "listtable",
    "listoverridetable",
    "revtbl",
    "fchars",
    "lchars",
    "pgdsctbl",
    "userprops",
    "wgrffmtfilter",
    "panose",
    "falt",
    "fontemb",
    "fontfile",
    "bkmkstart",
    "bkmkend",
    "pnseclvl",
    "template",
    "filetbl",
    "mmathpr",
    "ftnsep",
    "ftnsepc",
    "aftnsep",
    "aftnsepc",
    "fldinst",
    "atnid",
    "atnauthor",
    "atndate",
    "atntime",
    "atnref",
    "atrfstart",
    "atrfend",
    "background",
    "docvar",
    "private",
    "xmlopen",
    "xmlclose",
    "factoidname",
    "shpinst",
    "sp",
    "picprop",
    "blipuid"
  )

  /** Destinations skipped because Serenity cannot hold them, mapped to the name reported through fidelity. */
  private val UnsupportedDestinations: Map[String, String] = Map(
    "pict"       -> "picture",
    "object"     -> "embedded object",
    "shp"        -> "shape",
    "footnote"   -> "footnote",
    "header"     -> "header/footer",
    "headerl"    -> "header/footer",
    "headerr"    -> "header/footer",
    "headerf"    -> "header/footer",
    "footer"     -> "header/footer",
    "footerl"    -> "header/footer",
    "footerr"    -> "header/footer",
    "footerf"    -> "header/footer",
    "listtext"   -> "list",
    "pntext"     -> "list",
    "pn"         -> "list",
    "annotation" -> "comment"
  )

  private val WalkedIgnorableDestinations: Set[String] = Set("shppict", "nonshppict")

  private val SpecialCharacters: Map[String, String] = Map(
    "tab"       -> "\t",
    "emdash"    -> "\u2014",
    "endash"    -> "\u2013",
    "emspace"   -> "\u2003",
    "enspace"   -> "\u2002",
    "qmspace"   -> "\u2005",
    "bullet"    -> "\u2022",
    "lquote"    -> "\u2018",
    "rquote"    -> "\u2019",
    "ldblquote" -> "\u201c",
    "rdblquote" -> "\u201d",
    "zwj"       -> "\u200d",
    "zwnj"      -> "\u200c",
    "ltrmark"   -> "\u200e",
    "rtlmark"   -> "\u200f"
  )

  private val ParagraphBreakWords: Set[String] = Set("par", "cell", "nestcell", "sect")

  def read(root: RtfNode.Group): RichTextImport =
    val header          = RtfHeader.read(root)
    val (endState, end) = walk(root.children, RtfState.initial, RtfProgress.initial(header.defaultCodePage), header)
    val closed          = if end.runs.isEmpty then end else endParagraph(end, endState, header)
    val paragraphs =
      if closed.paragraphs.nonEmpty then closed.paragraphs.toList
      else List(RichTextParagraph.plain(""))
    RichTextImport(RichTextDocument(paragraphs).normalized, FidelityReport.unsupported(closed.unsupported))

  private def walk(
    nodes: Vector[RtfNode],
    state: RtfState,
    progress: RtfProgress,
    header: RtfHeader
  ): (RtfState, RtfProgress) =
    val (finalState, finalProgress) = nodes.foldLeft((state, progress)) {
      case ((current, soFar), node) =>
        step(node, current, soFar, header)
    }
    (finalState, flushBytes(finalProgress))

  private def step(node: RtfNode, state: RtfState, progress: RtfProgress, header: RtfHeader): (RtfState, RtfProgress) =
    node match
      case group: RtfNode.Group => (state, enterGroup(group, state, progress, header))
      case RtfNode.Text(value)  => (state, appendText(value, state, progress, header))
      case RtfNode.Hex(value)   => (state, appendByte(value, state, progress, header))
      case RtfNode.Control(name, parameter) =>
        control(name, parameter, state, endUnicodeSkip(progress), header)

  private def endUnicodeSkip(progress: RtfProgress): RtfProgress =
    progress.copy(skip = 0)

  private def enterGroup(group: RtfNode.Group, state: RtfState, progress: RtfProgress, header: RtfHeader): RtfProgress =
    val destination = RtfHeader.destinationOf(group)
    val ignorable   = RtfHeader.isIgnorable(group)
    destination match
      case Some(name) if UnsupportedDestinations.contains(name) =>
        progress.report(UnsupportedDestinations.getOrElse(name, name))
      case Some(name) if SilentDestinations.contains(name.toLowerCase) => progress
      case Some(name) if ignorable && !WalkedIgnorableDestinations.contains(name) =>
        progress.report(s"destination:$name")
      case _ => walk(group.children, state, progress, header)._2

  private def appendText(value: String, state: RtfState, progress: RtfProgress, header: RtfHeader): RtfProgress =
    val kept = value.drop(progress.skip)
    val next = progress.copy(skip = (progress.skip - value.length).max(0))
    if kept.isEmpty then next else emit(kept, state, next, header)

  private def appendByte(value: Int, state: RtfState, progress: RtfProgress, header: RtfHeader): RtfProgress =
    if progress.skip > 0 then progress.copy(skip = progress.skip - 1)
    else
      val codePage = header.codePageOf(effectiveChar(state, header).font)
      val style    = runStyle(state, header)
      val continuesPending =
        progress.pendingCodePage == codePage && progress.pendingStyle == style
      val ready = if continuesPending then progress else flushBytes(progress)
      ready.copy(pendingBytes = ready.pendingBytes :+ value.toByte, pendingCodePage = codePage, pendingStyle = style)

  private def flushBytes(progress: RtfProgress): RtfProgress =
    if progress.pendingBytes.isEmpty then progress
    else
      val text = String(progress.pendingBytes.toArray, RtfHeader.charsetFor(progress.pendingCodePage))
      val run  = RichTextRun(text, progress.pendingStyle)
      progress.copy(runs = progress.runs :+ run, pendingBytes = Vector.empty)

  private def emit(text: String, state: RtfState, progress: RtfProgress, header: RtfHeader): RtfProgress =
    val flushed = flushBytes(progress)
    flushed.copy(runs = flushed.runs :+ RichTextRun(text, runStyle(state, header)))

  private def emitSoftBreak(state: RtfState, progress: RtfProgress, header: RtfHeader): RtfProgress =
    val flushed = flushBytes(progress)
    flushed.copy(runs = flushed.runs :+ RichTextRun.softBreak(runStyle(state, header)))

  private def control(
    name: String,
    parameter: Option[Int],
    state: RtfState,
    progress: RtfProgress,
    header: RtfHeader
  ): (RtfState, RtfProgress) =
    if ParagraphBreakWords.contains(name) then (state, endParagraph(progress, state, header))
    else if name == "u" then unicodeEscape(parameter, state, progress, header)
    else if name == "line" then (state, emitSoftBreak(state, progress, header))
    else
      SpecialCharacters.get(name) match
        case Some(text) => (state, emit(text, state, progress, header))
        case None       => formatting(name, parameter, state, progress)

  private def unicodeEscape(
    parameter: Option[Int],
    state: RtfState,
    progress: RtfProgress,
    header: RtfHeader
  ): (RtfState, RtfProgress) =
    parameter match
      case Some(value) =>
        val emitted = emit((value & 0xffff).toChar.toString, state, progress, header)
        (state, emitted.copy(skip = state.unicodeSkip))
      case None => (state, progress)

  private def formatting(
    name: String,
    parameter: Option[Int],
    state: RtfState,
    progress: RtfProgress
  ): (RtfState, RtfProgress) =
    name match
      case "pard"                          => (state.resetParagraph, progress)
      case "plain"                         => (state.resetCharacter, progress)
      case "uc"                            => (state.copy(unicodeSkip = parameter.getOrElse(1).max(0)), progress)
      case "s"                             => (parameter.fold(state)(id => state.copy(paragraphStyle = id)), progress)
      case "cs"                            => (state.copy(characterStyle = parameter), progress)
      case "outlinelevel"                  => (state.copy(outlineLevel = parameter), progress)
      case "dropcapli"                     => (state.copy(dropCapLines = parameter.filter(_ > 0)), progress)
      case "trowd" | "intbl"               => (state, progress.report("table"))
      case "ls" if parameter.exists(_ > 0) => (state, progress.report("list"))
      case "field"                         => (state, progress.report("field"))
      case word if RtfHeader.alignmentOf(word).isDefined =>
        (state.copy(alignment = RtfHeader.alignmentOf(word)), progress)
      case word if RtfCharFormat.Unmodelled.contains(word) =>
        val feature = RtfCharFormat.Unmodelled.getOrElse(word, word)
        (state, if parameter.contains(0) then progress else progress.report(feature))
      case word =>
        state.char.withControl(word, parameter) match
          case Some(format) => (state.copy(char = format), progress)
          case None         => (state, progress)

  private def effectiveChar(state: RtfState, header: RtfHeader): RtfCharFormat =
    val paragraph     = header.resolvedParagraphStyles.getOrElse(state.paragraphStyle, RtfResolvedStyle.empty)
    val fromParagraph = if headingLevel(state, header).isDefined then RtfCharFormat.empty else paragraph.char
    val fromCharacter = state.characterStyle.flatMap(header.resolvedCharacterStyles.get).map(_.char)
    fromCharacter.fold(fromParagraph.overlay(state.char))(character =>
      fromParagraph.overlay(character).overlay(state.char)
    )

  private def runStyle(state: RtfState, header: RtfHeader): RichTextStyle =
    val format = effectiveChar(state, header)
    RichTextStyle(
      marks = Set(
        Option.when(format.bold.contains(true))(InlineMark.Bold),
        Option.when(format.italic.contains(true))(InlineMark.Italic),
        Option.when(format.underline.contains(true))(InlineMark.Underline)
      ).flatten,
      fontFamily = format.font.flatMap(header.fonts.get).map(_.name).filter(_.nonEmpty),
      fontSize = format.halfPoints.filter(_ > 0).map(_ / 2.0f),
      color = format.color.flatMap(header.colorAt)
    )

  private def headingLevel(state: RtfState, header: RtfHeader): Option[Int] =
    val style = header.resolvedParagraphStyles.getOrElse(state.paragraphStyle, RtfResolvedStyle.empty)
    state.outlineLevel
      .flatMap(headingOfOutline)
      .orElse(style.headingLevelByName)
      .orElse(style.outlineLevel.flatMap(headingOfOutline))

  /** RTF numbers outline levels 0 to 8 for headings; anything else (Word uses 9) is body text. */
  private def headingOfOutline(outlineLevel: Int): Option[Int] =
    Option.when(outlineLevel >= 0 && outlineLevel <= 8)(outlineLevel + 1)

  private def role(state: RtfState, header: RtfHeader): ParagraphRole =
    val style = header.resolvedParagraphStyles.getOrElse(state.paragraphStyle, RtfResolvedStyle.empty)
    state.dropCapLines
      .orElse(style.dropCapLines)
      .map(ParagraphRole.dropCap(_))
      .orElse(headingLevel(state, header).map(ParagraphRole.Heading(_)))
      .getOrElse(ParagraphRole.Body)

  private def endParagraph(progress: RtfProgress, state: RtfState, header: RtfHeader): RtfProgress =
    val flushed = flushBytes(progress)
    val style   = header.resolvedParagraphStyles.getOrElse(state.paragraphStyle, RtfResolvedStyle.empty)
    val paragraph = RichTextParagraph(
      runs = flushed.runs.toList,
      alignment = state.alignment.orElse(style.alignment).getOrElse(ParagraphAlignment.Left),
      role = role(state, header)
    ).normalized
    flushed.copy(paragraphs = flushed.paragraphs :+ paragraph, runs = Vector.empty)
