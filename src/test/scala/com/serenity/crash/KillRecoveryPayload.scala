package com.serenity.crash

/** The text the kill-recovery child writes and the spec recognises it by. Every version is a complete, distinct,
  * growing document, so a file holding anything other than one whole version is truncated or mixed.
  */
private[crash] object KillRecoveryPayload:

  val FileKind = "file"
  val HotKind  = "hot"

  def text(kind: String, version: Int): String =
    val lines = 40 + math.min(version, 400) * 20
    (s"$kind version $version" +: (0 until lines).map(line => s"$kind $version line $line ${"x" * 40}")).mkString("\n")

  /** The version `found` is a complete copy of, if it is one at all: the header names it, the whole text must match. */
  def versionOf(kind: String, found: String): Option[Int] =
    found.linesIterator
      .nextOption()
      .flatMap(_.stripPrefix(s"$kind version ").toIntOption)
      .filter(version => text(kind, version) == found)

  def fileAck(version: Int): String    = s"file $version"
  def sessionAck(version: Int): String = s"session $version"
