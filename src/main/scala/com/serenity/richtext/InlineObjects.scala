package com.serenity.richtext

/** Names what a piece of content kept without modelling is, for the fidelity report. */
private[richtext] object InlineObjects:
  private val SettingsPart = "settings part"

  private val DocxFeatures: List[(List[String], DocumentFeature)] = List(
    List("<w:drawing", "<w:pict", "<mc:AlternateContent")  -> DocumentFeature.Images,
    List("<w:object", "<w:embed")                          -> DocumentFeature.Embedded,
    List("<w:footnoteReference", "<w:endnoteReference")    -> DocumentFeature.Notes,
    List("<w:fldSimple", "<w:fldChar", "<w:instrText")     -> DocumentFeature.Fields,
    List("<w:ins ", "<w:del ", "<w:moveFrom", "<w:moveTo") -> DocumentFeature.TrackedChanges,
    List("<w:sdt")                                         -> DocumentFeature.ContentControls,
    List("<m:oMath")                                       -> DocumentFeature.Equations
  )

  private val OdtFeatures: List[(List[String], DocumentFeature)] = List(
    List("<draw:")     -> DocumentFeature.Images,
    List("<text:note") -> DocumentFeature.Notes,
    List(
      "<text:date",
      "<text:time",
      "<text:page-",
      "<text:title",
      "<text:author",
      "<text:file-name",
      "<text:sequence",
      "<text:variable",
      "<text:user-field",
      "<text:bibliography-mark",
      "<text:reference-ref"
    )                    -> DocumentFeature.Fields,
    List("<text:change") -> DocumentFeature.TrackedChanges
  )

  /** What the visible inline XML `raw` is, by the elements it holds. */
  def feature(raw: String): DocumentFeature =
    (DocxFeatures ++ OdtFeatures)
      .collectFirst { case (markers, feature) if markers.exists(raw.contains) => feature }
      .getOrElse(DocumentFeature.Other("inline object"))

  /** Whether `feature` is what a package part holds, as opposed to content found in the document body. */
  def isPartFeature(feature: DocumentFeature): Boolean =
    feature match
      case DocumentFeature.Styles | DocumentFeature.Lists | DocumentFeature.HeadersFooters | DocumentFeature.Comments =>
        true
      case DocumentFeature.Other(name) => name == SettingsPart
      case _                           => false

  /** What a package entry holds that the model does not, or `None` for the entries every save writes or that belong to
    * content counted on its own (an image's media file).
    */
  def partFeature(format: PackageFormat, name: String): Option[DocumentFeature] =
    if name.endsWith("/") then None
    else
      format match
        case PackageFormat.Docx => docxPart(name)
        case PackageFormat.Odt  => odtPart(name)

  private def docxPart(name: String): Option[DocumentFeature] =
    if name == "[Content_Types].xml" || name == "_rels/.rels" || name == DocxDocumentCodec.DocumentEntry ||
        name.contains("/_rels/") || name.startsWith("word/media/") || name.startsWith("word/embeddings/")
    then None
    else if name == "word/styles.xml" || name == "word/stylesWithEffects.xml" then Some(DocumentFeature.Styles)
    else if name == "word/numbering.xml" then Some(DocumentFeature.Lists)
    else if name.startsWith("word/header") || name.startsWith("word/footer") then Some(DocumentFeature.HeadersFooters)
    else if name == "word/footnotes.xml" || name == "word/endnotes.xml" then Some(DocumentFeature.Notes)
    else if name.startsWith("word/comments") then Some(DocumentFeature.Comments)
    else Some(DocumentFeature.Other(SettingsPart))

  private def odtPart(name: String): Option[DocumentFeature] =
    if name == "mimetype" || name == "content.xml" || name.startsWith("META-INF/") || name.startsWith("Pictures/") ||
        name.startsWith("Thumbnails/")
    then None
    else if name == "styles.xml" then Some(DocumentFeature.Styles)
    else Some(DocumentFeature.Other(SettingsPart))
