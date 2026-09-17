package com.serenity.animation.sprite

/** How a sprite steps through a sheet's frames once something (originally the retired window-sitter title-bar
  * decoration, now the companion sprite panel's typing reaction) triggers it -- letting a single small idle bob-cycle
  * (the bundled `CompanionCharacter.PixelWizard` placeholder sheet has only four frames) read as three visually
  * distinct animations without needing a separate sprite-sheet clip per style.
  */
enum SpriteFrameCycle(val configKey: String):
  case Cycle extends SpriteFrameCycle("cycle")
  case Pulse extends SpriteFrameCycle("pulse")
  case Blink extends SpriteFrameCycle("blink")

object SpriteFrameCycle:
  val default: SpriteFrameCycle = Pulse

  def fromConfigKey(value: String): Option[SpriteFrameCycle] =
    value.trim.toLowerCase match
      case "cycle" => Some(Cycle)
      case "pulse" => Some(Pulse)
      case "blink" => Some(Blink)
      case _       => None

/** Which of a sprite sheet's frames each [[SpriteFrameCycle]] plays, and in what order. Pure and stateless: callers
  * hold the current step (a position within the selected subset, not a raw sheet index) and pass it back in.
  */
object SpriteFrameSelection:

  final case class FrameStep(index: Int, ascending: Boolean)

  /** Frame indices (into the sheet's full frame vector) that `cycle` plays, in traversal order. Always non-empty, even
    * for a degenerate 0-or-1-frame sheet, so callers never need to guard against an empty selection.
    */
  def indices(cycle: SpriteFrameCycle, frameCount: Int): Vector[Int] =
    if frameCount <= 0 then Vector(0)
    else
      cycle match
        case SpriteFrameCycle.Cycle => Vector.range(0, frameCount)
        case SpriteFrameCycle.Pulse => Vector.range(0, frameCount).dropRight(if frameCount > 2 then 1 else 0)
        case SpriteFrameCycle.Blink => Vector(0, frameCount - 1).distinct

  /** The next step (a position within `indices(cycle, frameCount)`) after `currentIndex`, per `cycle`'s traversal:
    * Cycle plays every frame forward and wraps, Pulse ping-pongs holding the last frame, Blink toggles the first and
    * last selected position.
    */
  def next(cycle: SpriteFrameCycle, frameCount: Int, currentIndex: Int, ascending: Boolean): FrameStep =
    val selected = indices(cycle, frameCount)
    val size     = selected.size
    if size <= 1 then FrameStep(0, ascending)
    else
      val currentPosition = currentIndex % size
      cycle match
        case SpriteFrameCycle.Blink =>
          FrameStep(if currentPosition == 0 then size - 1 else 0, ascending)
        case SpriteFrameCycle.Cycle =>
          FrameStep((currentPosition + 1) % size, ascending)
        case SpriteFrameCycle.Pulse =>
          if ascending then
            if currentPosition >= size - 2 then FrameStep(size - 1, false)
            else FrameStep(currentPosition + 1, true)
          else if currentPosition <= 1 then FrameStep(0, true)
          else FrameStep(currentPosition - 1, false)

  /** Resolves a step's position (as stored by a caller like [[CompanionSpriteState]]) to the actual sheet frame index
    * to paint.
    */
  def resolve(cycle: SpriteFrameCycle, frameCount: Int, position: Int): Int =
    val selected = indices(cycle, frameCount)
    selected.lift(position % selected.size).getOrElse(0)
