package com.serenity.lsp.model

/** Lines `first` to `last`, both inclusive. */
final case class LineRange(first: Int, last: Int)

/** The request that brings a whole document's tokens up to date. */
enum PrimaryRequest:
  case Full

  /** Only what changed since the result `previousResultId` named. */
  case Delta(previousResultId: String)

/** Which semantic tokens requests to send for one refresh: `primary` covers the whole document, `preview` only the
  * lines on screen, answering sooner while a slow `primary` is pending or standing in for one a server does not offer.
  */
final case class SemanticTokensPlan(primary: Option[PrimaryRequest], preview: Option[LineRange])

object SemanticTokensPlan:

  /** A document with this many lines makes a whole-document answer slow enough to be worth a preview. */
  val LargeDocumentLines: Int = 5000

  /** The requests to send given what the server offers and what this client already holds. `delta` is used whenever the
    * server offers it and a previous result id is held. A server offering `range` but no `delta` is also asked for the
    * visible lines, as is any server on a large document that has no result to take a delta from yet; a server offering
    * `range` alone is asked for the visible lines, or the whole document when they are not known, and nothing else.
    */
  def choose(
    features: SemanticTokensFeatures,
    previousResultId: Option[String],
    lineCount: Int,
    visible: Option[LineRange]
  ): SemanticTokensPlan =
    val primary: Option[PrimaryRequest] =
      Option.when(features.full)(
        previousResultId.filter(_ => features.delta).fold[PrimaryRequest](PrimaryRequest.Full)(PrimaryRequest.Delta(_))
      )
    val previewWanted = features.range && primary.forall {
      case PrimaryRequest.Full     => !features.delta || lineCount >= LargeDocumentLines
      case PrimaryRequest.Delta(_) => false
    }
    val preview =
      if !previewWanted then None
      else if primary.isEmpty then Some(visible.getOrElse(LineRange(0, math.max(0, lineCount - 1))))
      else visible
    SemanticTokensPlan(primary, preview)
