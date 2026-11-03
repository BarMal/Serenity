package com.serenity.diagnostics

/** What "presented" means in a timing line. A canvas paint is over when `paintComponent` returns. The Skia layer draws
  * on Skiko's own thread afterwards, and gives no GPU context to wait on, so its frame counts as presented at the end
  * of Skiko's render callback: not GPU completion, and not display scan-out.
  */
object PresentPoint:

  private val SkiaLayer = "skia"

  def of(backend: String): String = if backend == SkiaLayer then "render-callback-end" else "paint-end"

  /** The tag a timing line carries once the backend is known; nothing before then. */
  def tag(backend: String): String =
    if backend.isEmpty then "" else s" backend=$backend presented=${of(backend)}"
