package com.serenity.richtext

/** A kind of document content a format can hold that Serenity either models or keeps without modelling. */
enum DocumentFeature:
  case Tables
  case Lists
  case Sections
  case ContentControls
  case Equations
  case Indexes
  case Images
  case Fields
  case Notes
  case Comments
  case TrackedChanges
  case Styles
  case HeadersFooters
  case Headings
  case Embedded
  case Other(name: String)

  /** The noun for `count` of this feature, as "table" or "tables". */
  def noun(count: Int): String =
    val (singular, plural) = this match
      case DocumentFeature.Tables          => ("table", "tables")
      case DocumentFeature.Lists           => ("list", "lists")
      case DocumentFeature.Sections        => ("section", "sections")
      case DocumentFeature.ContentControls => ("content control", "content controls")
      case DocumentFeature.Equations       => ("equation", "equations")
      case DocumentFeature.Indexes         => ("index", "indexes")
      case DocumentFeature.Images          => ("image", "images")
      case DocumentFeature.Fields          => ("field", "fields")
      case DocumentFeature.Notes           => ("note", "notes")
      case DocumentFeature.Comments        => ("comment part", "comment parts")
      case DocumentFeature.TrackedChanges  => ("tracked change", "tracked changes")
      case DocumentFeature.Styles          => ("style sheet", "style sheets")
      case DocumentFeature.HeadersFooters  => ("header or footer", "headers and footers")
      case DocumentFeature.Headings        => ("heading", "headings")
      case DocumentFeature.Embedded        => ("embedded file", "embedded files")
      case DocumentFeature.Other(name)     => (name, name + "s")
    if count == 1 then singular else plural

object DocumentFeature:
  private val OtherPrefix = "Other:"

  private val Named: List[DocumentFeature] = List(
    Tables,
    Lists,
    Sections,
    ContentControls,
    Equations,
    Indexes,
    Images,
    Fields,
    Notes,
    Comments,
    TrackedChanges,
    Styles,
    HeadersFooters,
    Headings,
    Embedded
  )

  /** The stable name of the feature, for session files. */
  def key(feature: DocumentFeature): String =
    feature match
      case Other(name) => OtherPrefix + name
      case named       => named.toString

  def fromKey(key: String): Option[DocumentFeature] =
    if key.startsWith(OtherPrefix) then Some(Other(key.stripPrefix(OtherPrefix)))
    else Named.find(_.toString == key)

/** What a save does with a feature's content. */
enum Treatment:
  /** A block shown as a read-only line and written back byte for byte. */
  case ReadOnly

  /** Content the model does not hold, written back as the source had it. */
  case Preserved

  /** Content the target format keeps only in an approximate form. */
  case Converted

  /** Content that is not in the file this save writes. */
  case Dropped

  /** Content the user deleted. */
  case Removed

object Treatment:
  def key(treatment: Treatment): String = treatment.toString

  def fromKey(key: String): Option[Treatment] = Treatment.values.find(_.toString == key)

final case class FidelityItem(feature: DocumentFeature, treatment: Treatment, count: Int)

/** The formats a rich document can be saved as. */
enum SaveTarget:
  case Docx
  case Odt
  case Rtf
  case Markdown
  case PlainText

  /** The package format a save writes, for the targets that keep a source package. */
  def packageFormat: Option[PackageFormat] =
    this match
      case SaveTarget.Docx => Some(PackageFormat.Docx)
      case SaveTarget.Odt  => Some(PackageFormat.Odt)
      case _               => None

/** What saving a document as one format does with each feature, for the warning shown before the save. */
final case class FidelityReport(items: List[FidelityItem]):

  def wouldDrop: List[FidelityItem] = items.filter(_.treatment == Treatment.Dropped)

  def count(feature: DocumentFeature, treatment: Treatment): Int =
    items.filter(item => item.feature == feature && item.treatment == treatment).map(_.count).sum

  /** One line for the user: what the save keeps read-only, converts, drops and removes. Content the file keeps as it
    * was (an image) is left out: it needs no warning.
    */
  def summary: String =
    FidelityReport.Phrases.flatMap((treatment, words) => phrase(treatment, words)).mkString("; ")

  /** The items a save would drop, as a phrase such as "1 table and 2 images". */
  def dropSummary: String =
    FidelityReport.listed(wouldDrop)

  private def phrase(treatment: Treatment, words: String): Option[String] =
    Some(items.filter(_.treatment == treatment))
      .filter(_.nonEmpty)
      .map(found => s"${FidelityReport.listed(found)} $words")

object FidelityReport:
  val empty: FidelityReport = FidelityReport(Nil)

  private val Phrases: List[(Treatment, String)] = List(
    Treatment.ReadOnly  -> "preserved read-only",
    Treatment.Converted -> "converted",
    Treatment.Dropped   -> "dropped",
    Treatment.Removed   -> "removed"
  )

  /** "1 table", "1 table and 2 images", "1 table, 2 images and 3 fields". */
  def listed(items: List[FidelityItem]): String =
    val parts = items.map(item => s"${item.count} ${item.feature.noun(item.count)}")
    parts match
      case Nil           => ""
      case single :: Nil => single
      case _             => parts.dropRight(1).mkString(", ") + " and " + parts.lastOption.getOrElse("")

  /** What saving `document` as `target` does with each feature. A save keeps what the model does not hold only into the
    * package format the document was read from, and only while that package is still there to copy from.
    */
  def forSave(document: RichTextDocument, target: SaveTarget): FidelityReport =
    val source      = document.source
    val passthrough = source.exists(origin => target.packageFormat.contains(origin.format))
    val paragraphs  = document.paragraphs
    val blocks      = paragraphs.flatMap(_.opaqueBlock)
    val blockEntries = blocks.map(block =>
      block.feature -> (if passthrough && block.raw.nonEmpty then Treatment.ReadOnly else Treatment.Dropped)
    )
    val inlineEntries = paragraphs
      .flatMap(_.runs.flatMap(_.atom))
      .collect { case InlineAtom.Opaque(raw, true) => InlineObjects.feature(raw) }
      .map(_ -> (if passthrough then Treatment.Preserved else Treatment.Dropped))
    val partEntries = source.toList.flatMap(origin =>
      origin.entryNames.toList.flatMap(InlineObjects.partFeature(origin.format, _)).map(_ -> partTreatment(passthrough))
    )
    val convertedEntries =
      Option
        .when(target == SaveTarget.Rtf)(paragraphs.filter(_.role.isInstanceOf[ParagraphRole.Heading]))
        .toList
        .flatten
        .map(_ => DocumentFeature.Headings -> Treatment.Converted)
    val removedEntries = removedBlocks(source, blocks)
    FidelityReport(
      tally(
        blockEntries ++ inlineEntries ++ partEntries ++ convertedEntries ++ removedEntries.map(_ -> Treatment.Removed)
      )
    )

  /** [[forSave]] for a document that was read from a package that is no longer available to copy from, which loses the
    * parts of the package too, though there is no telling which.
    */
  def forDetached(document: RichTextDocument, target: SaveTarget): FidelityReport =
    val report = forSave(document.withSource(None), target)
    FidelityReport(report.items :+ FidelityItem(DocumentFeature.Other("document part"), Treatment.Dropped, 1))

  private def partTreatment(passthrough: Boolean): Treatment =
    if passthrough then Treatment.Preserved else Treatment.Dropped

  /** The features of the source's blocks that no line holds any more, one entry per missing block. */
  private def removedBlocks(source: Option[DocumentSource], present: List[InlineAtom.Block]): List[DocumentFeature] =
    val original =
      source.flatMap(_.body).toList.flatMap(_.blocks).map(_.kind).collect { case BodyKind.Block(feature) => feature }
    val remaining = present.groupMapReduce(_.feature)(_ => 1)(_ + _)
    original
      .groupBy(identity)
      .toList
      .sortBy((feature, _) => original.indexOf(feature))
      .flatMap((feature, found) => List.fill((found.size - remaining.getOrElse(feature, 0)).max(0))(feature))

  /** The entries as items, in order of first appearance, with the entries that agree counted together. */
  private def tally(entries: List[(DocumentFeature, Treatment)]): List[FidelityItem] =
    entries.distinct.map((feature, treatment) =>
      FidelityItem(feature, treatment, entries.count(_ == (feature -> treatment)))
    )
