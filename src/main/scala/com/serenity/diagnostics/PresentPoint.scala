package com.serenity.diagnostics

/** What "presented" means in a timing line. A canvas paint is over when `paintComponent` returns. The Skia layer draws
  * on the event thread too, but its render callback only records: Skiko replays the picture on the GPU canvas, flushes
  * and swaps afterwards. Its frame counts as presented at the end of that draw cycle: not GPU completion, and not
  * display scan-out.
  */
object PresentPoint:

  private val SkiaLayer = "skia"

  def of(backend: String): String = if backend == SkiaLayer then "draw-cycle-end" else "paint-end"

  /** The tag a timing line carries once the backend is known; nothing before then. */
  def tag(backend: String): String =
    if backend.isEmpty then "" else s" backend=$backend presented=${of(backend)}"
