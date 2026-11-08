package com.serenity.host

import java.nio.charset.StandardCharsets
import java.nio.file.Path
import java.security.MessageDigest
import java.util.HexFormat

/** The project a server host belongs to. The caller passes the real path of the trusted root, so two spellings of one
  * folder share a host; this only normalises what it is given.
  */
opaque type HostKey = Path

object HostKey:

  private val IdBytes = 8

  def apply(root: Path): HostKey = root.toAbsolutePath.normalize

  extension (key: HostKey)
    def root: Path = key

    /** A short, file-name-safe name for the host's directory, stable across runs. */
    def id: String =
      val digest = MessageDigest.getInstance("SHA-256").digest(key.toString.getBytes(StandardCharsets.UTF_8))
      HexFormat.of().formatHex(digest, 0, IdBytes)
