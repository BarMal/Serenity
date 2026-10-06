package com.serenity.richtext

import scala.annotation.tailrec

private[richtext] enum RtfNode:
  case Group(children: Vector[RtfNode])
  case Control(name: String, parameter: Option[Int])
  case Hex(value: Int)
  case Text(value: String)

/** Builds the group tree of an RTF document. Unknown destinations stay in the tree as ordinary groups, so a reader can
  * skip them as opaque units without having to understand them.
  */
private[richtext] object RtfParser:
  /** Deeper nesting than any real document; bounds the recursion of everything that walks the tree. */
  val MaxNestingDepth: Int = 256

  def parse(bytes: Array[Byte]): Either[RichTextCodecException, RtfNode.Group] =
    for
      tokens <- RtfTokenizer.tokenize(bytes)
      roots  <- build(tokens, 0, List(Vector.empty))
      root   <- roots.collectFirst { case group: RtfNode.Group => group }.toRight(notRtf)
      _      <- Either.cond(isRtfHeader(root), (), notRtf)
    yield root

  private def notRtf: RichTextCodecException =
    RichTextCodecException("Not an RTF document: it does not start with {\\rtf")

  private def isRtfHeader(root: RtfNode.Group): Boolean =
    root.children.headOption.exists {
      case RtfNode.Control("rtf", _) => true
      case _                         => false
    }

  @tailrec
  private def build(
    tokens: Vector[RtfToken],
    index: Int,
    open: List[Vector[RtfNode]]
  ): Either[RichTextCodecException, Vector[RtfNode]] =
    if index >= tokens.length then
      open match
        case top :: Nil => Right(top)
        case _ =>
          Left(RichTextCodecException(s"RTF document is truncated: ${open.length - 1} group(s) are never closed"))
    else
      tokens(index) match
        case RtfToken.GroupStart =>
          if open.length > MaxNestingDepth then
            Left(RichTextCodecException(s"RTF document nests groups deeper than $MaxNestingDepth levels"))
          else build(tokens, index + 1, Vector.empty :: open)
        case RtfToken.GroupEnd =>
          open match
            case finished :: parent :: rest => build(tokens, index + 1, (parent :+ RtfNode.Group(finished)) :: rest)
            case _ => Left(RichTextCodecException("RTF document has a closing brace with no matching opening brace"))
        case token =>
          open match
            case current :: rest => build(tokens, index + 1, (current ++ nodesFor(token)) :: rest)
            case Nil             => Left(RichTextCodecException("RTF parser lost its root"))

  private def nodesFor(token: RtfToken): Vector[RtfNode] =
    token match
      case RtfToken.Word(name, parameter)          => Vector(RtfNode.Control(name, parameter))
      case RtfToken.HexByte(value)                 => Vector(RtfNode.Hex(value))
      case RtfToken.Text(value)                    => Vector(RtfNode.Text(value))
      case RtfToken.Symbol(char)                   => symbolNodes(char)
      case RtfToken.GroupStart | RtfToken.GroupEnd => Vector.empty

  private def symbolNodes(char: Char): Vector[RtfNode] =
    char match
      case '\\' | '{' | '}' => Vector(RtfNode.Text(char.toString))
      case '~'              => Vector(RtfNode.Text("\u00a0"))
      case '_'              => Vector(RtfNode.Text("\u2011"))
      case '*'              => Vector(RtfNode.Control("*", None))
      case _                => Vector.empty
