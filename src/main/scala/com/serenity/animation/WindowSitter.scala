package com.serenity.animation

/** Immutable decorative state for the small animated character shown in custom window chrome. */
case class WindowSitter(
    frames: Vector[String],
    frameIndex: Int = 0,
    activeTicks: Int = 0,
    lastTypedAtNanos: Option[Long] = None
):

  /** Whether the sitter should continue receiving animation ticks. */
  def isActive: Boolean = activeTicks > 0

  /** Current decorative glyph. */
  def glyph: String = frames.lift(frameIndex).getOrElse(frames.headOption.getOrElse("·"))

  /** React to printable input, using the interval since the previous character as activity intensity. */
  def observeTyping(nowNanos: Long): WindowSitter =
    val intervalNanos = lastTypedAtNanos.map(previous => (nowNanos - previous).max(0L))
    val ticks         = intervalNanos.fold(8L)(interval => if interval <= 150_000_000L then 16L else 8L)
    copy(
      frameIndex = if frames.isEmpty then 0 else (frameIndex + 1) % frames.size,
      activeTicks = ticks.toInt,
      lastTypedAtNanos = Some(nowNanos)
    )

  /** Advance one animation tick, returning to the resting glyph after the activity window. */
  def advance: WindowSitter =
    if !isActive then this
    else if activeTicks == 1 then copy(frameIndex = 0, activeTicks = 0)
    else copy(frameIndex = if frames.isEmpty then 0 else (frameIndex + 1) % frames.size, activeTicks = activeTicks - 1)

object WindowSitter:
  val default: WindowSitter = WindowSitter(Vector("·", "o", "O", "o"))
