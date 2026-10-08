package com.serenity.richtext

import java.nio.charset.StandardCharsets

import com.serenity.richtext.XmlDom.{elements, escapeAttribute}

/** Hyperlink relationships of a document part: the ones it already has, and the ones a save has to add. */
final private[richtext] case class DocxLinks(ids: Map[String, String], additions: List[String]):

  /** `relationships` with the added entries, or `None` when nothing was added. */
  def withAdditionsIn(relationships: Option[String]): Option[String] =
    Option.when(additions.nonEmpty) {
      relationships.fold(DocxLinks.relationshipsDocument(additions)) { existing =>
        val close = existing.lastIndexOf("</Relationships>")
        if close < 0 then DocxLinks.relationshipsDocument(additions)
        else existing.substring(0, close) + additions.mkString("\n") + "\n" + existing.substring(close)
      }
    }

private[richtext] object DocxLinks:
  private val PkgRel                = "http://schemas.openxmlformats.org/package/2006/relationships"
  private val HyperlinkRelationship = "http://schemas.openxmlformats.org/officeDocument/2006/relationships/hyperlink"

  final private case class Existing(hyperlinks: Map[String, String], allIds: Set[String])

  /** Relationship id to external target, for the relationships that are hyperlinks. */
  def hyperlinkTargets(relationships: Array[Byte]): Map[String, String] =
    existing(RichTextXmlParser.parse(relationships)).hyperlinks

  /** Ids for `targets`: the id an existing hyperlink relationship already has, or a fresh one that clashes with none.
    */
  def allocate(targets: List[String], relationships: Option[String]): DocxLinks =
    val known = relationships
      .map(text => existing(RichTextXmlParser.parse(text.getBytes(StandardCharsets.UTF_8))))
      .getOrElse(Existing(Map.empty, Set.empty))
    val byTarget = known.hyperlinks.toList.sortBy(_._1).reverseIterator.map((id, target) => target -> id).toMap
    val missing  = targets.filterNot(_.startsWith("#")).distinct.filterNot(byTarget.contains)
    val freshIds =
      Iterator.from(1).map(number => s"rId$number").filterNot(known.allIds.contains).take(missing.size).toList
    val added = missing.zip(freshIds).toMap
    DocxLinks(
      byTarget ++ added,
      missing.map(target => relationshipXml(added(target), target))
    )

  def relationshipsDocument(relationships: List[String]): String =
    s"""<?xml version="1.0" encoding="UTF-8"?>
       |<Relationships xmlns="$PkgRel">
       |${relationships.mkString("\n")}
       |</Relationships>""".stripMargin

  private def relationshipXml(id: String, target: String): String =
    s"""  <Relationship Id="$id" Type="$HyperlinkRelationship" Target="${escapeAttribute(target)}" TargetMode="External"/>"""

  private def existing(document: org.w3c.dom.Document): Existing =
    val relationships = elements(document.getElementsByTagNameNS(PkgRel, "Relationship"))
    Existing(
      relationships
        .filter(_.getAttribute("Type") == HyperlinkRelationship)
        .flatMap(relationship =>
          Option(relationship.getAttribute("Id"))
            .filter(_.nonEmpty)
            .zip(Option(relationship.getAttribute("Target")).filter(_.nonEmpty))
        )
        .toMap,
      relationships.flatMap(relationship => Option(relationship.getAttribute("Id")).filter(_.nonEmpty)).toSet
    )
