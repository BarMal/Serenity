package com.serenity.richtext

import scala.annotation.tailrec

import org.w3c.dom.{Element, Node, NodeList}

/** Small navigation helpers over DOM elements, shared by the format codecs. */
private[richtext] object XmlDom:

  def attribute(element: Element, namespace: String, localName: String): Option[String] =
    Option(element.getAttributeNS(namespace, localName)).filter(_.nonEmpty)

  def isElement(node: Node, namespace: String, localName: String): Boolean =
    node.getNamespaceURI == namespace && node.getLocalName == localName

  def childElement(element: Element, namespace: String, localName: String): Option[Element] =
    childElements(element).find(isElement(_, namespace, localName))

  def childElements(element: Element): List[Element] =
    nodes(element.getChildNodes).collect { case child: Element => child }

  def nodes(nodeList: NodeList): List[Node] =
    (0 until nodeList.getLength).toList.map(nodeList.item)

  def elements(nodeList: NodeList): List[Element] =
    nodes(nodeList).collect { case element: Element => element }

  /** Every element at or below `root`, in document order. */
  def preorder(root: Element): Vector[Element] =
    @tailrec
    def walk(pending: List[Element], found: Vector[Element]): Vector[Element] =
      pending match
        case Nil             => found
        case head :: remains => walk(childElements(head) ::: remains, found :+ head)
    walk(List(root), Vector.empty)

  def escapeText(value: String): String =
    value
      .replace("&", "&amp;")
      .replace("<", "&lt;")
      .replace(">", "&gt;")

  def escapeAttribute(value: String): String =
    escapeText(value).replace("\"", "&quot;")

/** Where each DOM element of a part sits in the part's source text, so unmodelled XML can be kept verbatim. */
final private[richtext] class SourceMap private (spans: XmlSpans, indexes: Map[Element, Int]):

  private def index(element: Element): Option[Int] = indexes.get(element)

  def raw(element: Element): Option[String]      = index(element).map(spans.raw)
  def openTag(element: Element): Option[String]  = index(element).map(spans.openTag)
  def closeTag(element: Element): Option[String] = index(element).map(spans.closeTag)

  def qualifiedName(element: Element): Option[String] =
    index(element).flatMap(spans.spans.lift).map(_.qualifiedName)

  /** The body children cut into slices covering the whole text between the body's start and end tags. */
  def bodySource(body: Element, kindOf: Element => BodyKind): Option[BodySource] =
    index(body).filterNot(spans.spans(_).selfClosing).map { bodyIndex =>
      val tailStart = spans.spans(bodyIndex).closeStart(spans.text)
      val children  = XmlDom.childElements(body).flatMap(child => index(child).map(child -> _))
      val starts    = children.map((_, childIndex) => spans.spans(childIndex).start) :+ tailStart
      val blocks = children.zip(starts.drop(1)).map {
        case ((child, childIndex), blockEnd) =>
          val span = spans.spans(childIndex)
          BodyBlock(spans.text.substring(span.start, blockEnd), span.end - span.start, kindOf(child))
      }
      BodySource(
        spans.text.substring(0, starts.headOption.getOrElse(tailStart)),
        blocks.toVector,
        spans.text.substring(tailStart)
      )
    }

private[richtext] object SourceMap:

  /** `None` when the DOM and the scanned text disagree about the elements, which would make every slice suspect. */
  def of(root: Element, text: String): Option[SourceMap] =
    val spans    = XmlSpans.scan(text)
    val elements = XmlDom.preorder(root)
    Option.when(
      elements.size == spans.spans.size &&
        elements.zip(spans.spans).forall((element, span) => element.getTagName == span.qualifiedName)
    )(SourceMap(spans, elements.zipWithIndex.toMap))
