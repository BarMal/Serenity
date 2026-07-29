package com.serenity.animation

/** Selects how the sitter advances through its configured frames. */
enum WindowSitterAction(val configKey: String):
  case Cycle extends WindowSitterAction("cycle")
  case Pulse extends WindowSitterAction("pulse")
  case Blink extends WindowSitterAction("blink")

object WindowSitterAction:

  def fromConfigKey(value: String): Option[WindowSitterAction] =
    value.trim.toLowerCase match
      case "cycle" => Some(Cycle)
      case "pulse" => Some(Pulse)
      case "blink" => Some(Blink)
      case _       => None

/** Persisted controls for the typing-reactive window sitter. */
case class WindowSitterConfig(
    enabled: Boolean = true,
    action: WindowSitterAction = WindowSitterAction.Pulse,
    frames: Vector[String] = Vector("·", "o", "O", "o"),
    activeTicks: Int = 8,
    fastActiveTicks: Int = 16,
    fastTypingThresholdMs: Int = 150
):

  def normalized: WindowSitterConfig =
    val normalizedFrames = frames.map(_.trim).filter(_.nonEmpty).take(32)
    copy(
      frames = if normalizedFrames.nonEmpty then normalizedFrames else Vector("·"),
      activeTicks = activeTicks.max(1).min(120),
      fastActiveTicks = fastActiveTicks.max(1).min(240),
      fastTypingThresholdMs = fastTypingThresholdMs.max(1).min(5000)
    )

object WindowSitterConfig:
  val default: WindowSitterConfig = WindowSitterConfig()

/** Immutable decorative state for the small animated character shown in custom window chrome. */
case class WindowSitter(
    frames: Vector[String],
    frameIndex: Int = 0,
    activeTicks: Int = 0,
    lastTypedAtNanos: Option[Long] = None,
    action: WindowSitterAction = WindowSitterAction.Pulse
):

  /** Whether the sitter should continue receiving animation ticks. */
  def isActive: Boolean = activeTicks > 0

  /** Current decorative glyph. */
  def glyph: String = frames.lift(frameIndex).getOrElse(frames.headOption.getOrElse("·"))

  /** React to printable input, using the interval since the previous character as activity intensity. */
  def observeTyping(nowNanos: Long, settings: WindowSitterConfig = WindowSitterConfig.default): WindowSitter =
    val normalized    = settings.normalized
    val intervalNanos = lastTypedAtNanos.map(previous => (nowNanos - previous).max(0L))
    val ticks = intervalNanos.fold(normalized.activeTicks) { interval =>
      if interval <= normalized.fastTypingThresholdMs.toLong * 1_000_000L then normalized.fastActiveTicks
      else normalized.activeTicks
    }
    copy(
      frames = normalized.frames,
      frameIndex = if normalized.frames.isEmpty then 0 else (frameIndex + 1) % normalized.frames.size,
      activeTicks = ticks,
      lastTypedAtNanos = Some(nowNanos),
      action = normalized.action
    )

  /** Advance one animation tick, returning to the resting glyph after the activity window. */
  def advance: WindowSitter =
    if !isActive then this
    else if activeTicks == 1 then copy(frameIndex = 0, activeTicks = 0)
    else copy(frameIndex = nextFrameIndex, activeTicks = activeTicks - 1)

  private def nextFrameIndex: Int =
    if frames.isEmpty then 0
    else
      action match
        case WindowSitterAction.Blink                            => if frameIndex == 0 then frames.size - 1 else 0
        case WindowSitterAction.Cycle | WindowSitterAction.Pulse => (frameIndex + 1) % frames.size

object WindowSitter:
  val default: WindowSitter = fromConfig(WindowSitterConfig.default)

  def fromConfig(config: WindowSitterConfig): WindowSitter =
    val normalized = config.normalized
    WindowSitter(normalized.frames, action = normalized.action)
