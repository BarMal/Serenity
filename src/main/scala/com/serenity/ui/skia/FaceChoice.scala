package com.serenity.ui.skia

/** Which face of a family draws a font of the weight and slant asked for. A family reports only bold and italic as its
  * style flags, so one with a dozen non-bold weights has a dozen candidates; the weight decides among them the way CSS
  * font matching does, and a face of the wrong slant is used only when the family has none of the right one.
  */
private[skia] object FaceChoice:

  final case class Candidate[A](face: A, weight: Int, italic: Boolean)

  val RegularWeight = 400
  val BoldWeight    = 700

  def choose[A](candidates: Vector[Candidate[A]], bold: Boolean, italic: Boolean): Option[A] =
    val sameSlant = candidates.filter(_.italic == italic)
    val pool      = if sameSlant.nonEmpty then sameSlant else candidates
    val target    = if bold then BoldWeight else RegularWeight
    pool.minByOption(candidate => preference(candidate.weight, target)).map(_.face)

  /** Smaller is better: the weights CSS tries first, nearest first. From regular to medium, `target` up to 500, then
    * lighter ones, then heavier; below regular, lighter then heavier; above medium, heavier then lighter.
    */
  private def preference(weight: Int, target: Int): (Int, Int) =
    if target >= RegularWeight && target <= 500 then
      if weight >= target && weight <= 500 then (0, weight)
      else if weight < target then (1, -weight)
      else (2, weight)
    else if target < RegularWeight then if weight <= target then (0, -weight) else (1, weight)
    else if weight >= target then (0, weight)
    else (1, -weight)
