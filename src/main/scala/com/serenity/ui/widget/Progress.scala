package com.serenity.ui.widget

/** How far along some work is: a known fraction, or just that it is under way. */
enum Progress:
  case Determinate(fraction: Double)
  case Indeterminate

/** Cell-grid drawings of progress, identical in the GUI and the terminal: every glyph here is in the block-elements and
  * Braille ranges both render.
  */
object ProgressGlyphs:

  private val Eighths = Vector("", "▏", "▎", "▍", "▌", "▋", "▊", "▉")
  private val Full    = "█"
  private val Track   = "░"
  private val Spinner = Vector("⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏")

  /** A bar exactly `width` cells wide. An indeterminate bar is a short block sweeping along the track, placed by
    * `frame` so an animation tick moves it.
    */
  def bar(progress: Progress, width: Int, frame: Long = 0L): String =
    val cells = math.max(1, width)
    progress match
      case Progress.Determinate(fraction) =>
        val eighths   = math.round(fraction.max(0.0).min(1.0) * cells * 8).toInt
        val full      = eighths / 8
        val remainder = if full < cells then Eighths(eighths % 8) else ""
        val filled    = Full * full + remainder
        filled + Track * (cells - full - remainder.length)
      case Progress.Indeterminate =>
        val blockWidth = math.max(1, cells / 5)
        val travel     = cells - blockWidth + 1
        val start      = Math.floorMod(frame, travel.toLong).toInt
        Track * start + Full * blockWidth + Track * (cells - start - blockWidth)

  def spinner(frame: Long): String = Spinner(Math.floorMod(frame, Spinner.size.toLong).toInt)

  /** A percentage label, or `None` for indeterminate work. */
  def percent(progress: Progress): Option[String] =
    progress match
      case Progress.Determinate(fraction) => Some(s"${math.round(fraction.max(0.0).min(1.0) * 100)}%")
      case Progress.Indeterminate         => None

/** Something a widget shows once it has it: still loading, loaded, loaded with nothing in it, or failed. */
enum Loadable[+A]:
  case Loading(progress: Option[Progress] = None)
  case Ready(value: A)
  case Empty(message: String)
  case Failed(reason: String)

  def toOption: Option[A] =
    this match
      case Ready(value) => Some(value)
      case _            => None

  def map[B](f: A => B): Loadable[B] =
    this match
      case Ready(value)      => Ready(f(value))
      case Loading(progress) => Loading(progress)
      case Empty(message)    => Empty(message)
      case Failed(reason)    => Failed(reason)
