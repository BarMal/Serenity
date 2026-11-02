package com.serenity.ui.skia

/** Which face of a family draws an AWT font. Weight is not part of the style a set reports (only bold and italic are),
  * so a family with a dozen non-bold weights has a dozen candidates; the one that advances as AWT's font does is the
  * one layout measured with. Faces of a worse style are never measured.
  */
private[skia] object FaceChoice:

  final case class Candidate[A](face: A, bold: Boolean, italic: Boolean, advance: () => Double)

  def choose[A](
    candidates: Vector[Candidate[A]],
    bold: Boolean,
    italic: Boolean,
    reference: Option[Double]
  ): Option[A] =
    def mismatch(candidate: Candidate[A]): Int =
      (if candidate.bold == bold then 0 else 1) + (if candidate.italic == italic then 0 else 1)
    val best     = candidates.map(mismatch).minOption
    val matching = candidates.filter(candidate => best.contains(mismatch(candidate)))
    reference
      .flatMap(target => matching.minByOption(candidate => math.abs(candidate.advance() - target)))
      .orElse(matching.headOption)
      .map(_.face)
