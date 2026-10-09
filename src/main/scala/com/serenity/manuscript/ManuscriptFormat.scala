package com.serenity.manuscript

/** Page sizes in twentieths of a point (twips), the unit DOCX page geometry is written in. */
enum PaperSize(val key: String, val widthTwips: Int, val heightTwips: Int):
  case Letter extends PaperSize("letter", 12240, 15840)
  case A4     extends PaperSize("a4", 11906, 16838)

object PaperSize:
  def fromKey(key: String): Option[PaperSize] =
    PaperSize.values.find(_.key == key.trim.toLowerCase)

/** How a manuscript looks on the page. Every standard-manuscript number lives in the presets below and nowhere else.
  *
  * `lineSpacing` is in 240ths of a line (480 is double spacing); `runningHeader` takes `<$surname>`, `<$keyword>` and
  * `<$p>` (the page number). The drops are fractions of the text area left above a chapter heading and above the title,
  * counted below the contact block.
  */
final case class ManuscriptFormat(
    key: String,
    fontFamily: String,
    fontSizePoints: Int,
    paper: PaperSize,
    marginTwips: Int,
    headerMarginTwips: Int,
    lineSpacing: Int,
    firstLineIndentTwips: Int,
    runningHeader: String,
    sceneBreak: String,
    chapterDrop: Double,
    titleDrop: Double,
    defaultTransforms: List[TextTransform]
):
  def textWidthTwips: Int  = paper.widthTwips - 2 * marginTwips
  def textHeightTwips: Int = paper.heightTwips - 2 * marginTwips

object ManuscriptFormat:

  /** Shunn's modern Standard Manuscript Format: 12 pt Times New Roman, double-spaced, 1-inch margins, half-inch indent,
    * italics kept, curly quotes, `#` between scenes, `Surname / KEYWORD / page` top right.
    */
  val Modern: ManuscriptFormat = ManuscriptFormat(
    key = "modern",
    fontFamily = "Times New Roman",
    fontSizePoints = 12,
    paper = PaperSize.Letter,
    marginTwips = 1440,
    headerMarginTwips = 720,
    lineSpacing = 480,
    firstLineIndentTwips = 720,
    runningHeader = "<$surname> / <$keyword> / <$p>",
    sceneBreak = "#",
    chapterDrop = 1.0 / 3.0,
    titleDrop = 1.0 / 3.0,
    defaultTransforms = List(TextTransform.SmartPunctuation)
  )

  /** The classic, typewriter-era variant: Courier, straight quotes, `--` for an em dash, italics underlined. */
  val Classic: ManuscriptFormat = Modern.copy(
    key = "classic",
    fontFamily = "Courier New",
    defaultTransforms =
      List(TextTransform.EmDashAsDoubleHyphen, TextTransform.StraightPunctuation, TextTransform.ItalicsAsUnderline)
  )

  val all: List[ManuscriptFormat] = List(Modern, Classic)

  def fromKey(key: String): Option[ManuscriptFormat] =
    all.find(_.key == key.trim.toLowerCase)
