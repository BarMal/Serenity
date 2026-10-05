package com.serenity.richtext

import java.io.ByteArrayInputStream
import javax.xml.XMLConstants
import javax.xml.parsers.{DocumentBuilder, DocumentBuilderFactory}

import scala.annotation.tailrec
import scala.util.control.NonFatal

import org.w3c.dom.{Document as XmlDocument, Element, NodeList}
import org.xml.sax.SAXException

/** Parses rich-text XML with the protections required for untrusted document content. */
private[richtext] object RichTextXmlParser:
  private val DisallowDoctypeDecl       = "http://apache.org/xml/features/disallow-doctype-decl"
  private val ExternalGeneralEntities   = "http://xml.org/sax/features/external-general-entities"
  private val ExternalParameterEntities = "http://xml.org/sax/features/external-parameter-entities"

  def parse(bytes: Array[Byte]): XmlDocument =
    val input = ByteArrayInputStream(bytes)
    try secureBuilder().parse(input)
    finally input.close()

  /** Local names of every element at or below `root` (restricted to `namespace` when given), from one linear walk. */
  def elementNames(root: Element, namespace: Option[String]): Set[String] =
    @tailrec
    def walk(pending: List[Element], names: Set[String]): Set[String] =
      pending match
        case Nil => names
        case element :: rest =>
          val next =
            if namespace.forall(_ == element.getNamespaceURI) then names + element.getLocalName else names
          walk(childElements(element.getChildNodes) ::: rest, next)
    walk(List(root), Set.empty)

  private def childElements(nodes: NodeList): List[Element] =
    (0 until nodes.getLength).toList.map(nodes.item).collect { case element: Element => element }

  private def secureBuilder(): DocumentBuilder =
    try
      val factory = DocumentBuilderFactory.newInstance()
      factory.setNamespaceAware(true)
      factory.setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, true)
      factory.setFeature(DisallowDoctypeDecl, true)
      factory.setFeature(ExternalGeneralEntities, false)
      factory.setFeature(ExternalParameterEntities, false)
      factory.setAttribute(XMLConstants.ACCESS_EXTERNAL_DTD, "")
      factory.setAttribute(XMLConstants.ACCESS_EXTERNAL_SCHEMA, "")
      factory.setXIncludeAware(false)
      factory.setExpandEntityReferences(false)

      val builder = factory.newDocumentBuilder()
      builder.setEntityResolver((_, _) => throw SAXException("External entity resolution is disabled"))
      builder.setErrorHandler(RichTextArchive.SilentXmlErrorHandler)
      builder
    catch
      case error: RichTextCodecException => throw error
      case NonFatal(error)               => throw RichTextCodecException("XML parser could not be configured", error)
