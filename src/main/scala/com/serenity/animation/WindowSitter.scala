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
final case class WindowSitterConfig(
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
final case class WindowSitter(
    frames: Vector[String],
    frameIndex: Int = 0,
    activeTicks: Int = 0,
    lastTypedAtNanos: Option[Long] = None,
    action: WindowSitterAction = WindowSitterAction.Pulse,
    pulseAscending: Boolean = true
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
    val next = nextFrame(
      frames = normalized.frames,
      action = normalized.action,
      ascending = if normalized.action == action then pulseAscending else true
    )
    copy(
      frames = normalized.frames,
      frameIndex = next.index,
      activeTicks = ticks,
      lastTypedAtNanos = Some(nowNanos),
      action = normalized.action,
      pulseAscending = next.ascending
    )

  /** Advance one animation tick, returning to the resting glyph after the activity window. */
  def advance: WindowSitter =
    if !isActive then this
    else if activeTicks == 1 then copy(frameIndex = 0, activeTicks = 0)
    else
      val next = nextFrame(frames, action, pulseAscending)
      copy(frameIndex = next.index, activeTicks = activeTicks - 1, pulseAscending = next.ascending)

  private def nextFrame(
    frames: Vector[String],
    action: WindowSitterAction,
    ascending: Boolean
  ): WindowSitter.FrameStep =
    if frames.isEmpty then WindowSitter.FrameStep(0, ascending)
    else
      val currentIndex = frameIndex % frames.size
      action match
        case WindowSitterAction.Blink =>
          WindowSitter.FrameStep(if currentIndex == 0 then frames.size - 1 else 0, ascending)
        case WindowSitterAction.Cycle =>
          WindowSitter.FrameStep((currentIndex + 1) % frames.size, ascending)
        case WindowSitterAction.Pulse =>
          if frames.size == 1 then WindowSitter.FrameStep(0, true)
          else if ascending then
            if currentIndex >= frames.size - 2 then WindowSitter.FrameStep(frames.size - 1, false)
            else WindowSitter.FrameStep(currentIndex + 1, true)
          else if currentIndex <= 1 then WindowSitter.FrameStep(0, true)
          else WindowSitter.FrameStep(currentIndex - 1, false)

object WindowSitter:
  final private case class FrameStep(index: Int, ascending: Boolean)

  val default: WindowSitter = fromConfig(WindowSitterConfig.default)

  def fromConfig(config: WindowSitterConfig): WindowSitter =
    val normalized = config.normalized
    WindowSitter(normalized.frames, action = normalized.action)
