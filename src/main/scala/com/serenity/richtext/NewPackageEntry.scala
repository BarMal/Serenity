package com.serenity.richtext

import java.time.LocalDateTime
import java.util.zip.ZipEntry

/** The entries of a package Serenity writes from scratch, so the bytes depend on the document alone. */
private[richtext] object NewPackageEntry:

  /** The DOS epoch: a clock-stamped entry makes two encodes of one document differ when they straddle a DOS tick. */
  private val EntryTime = LocalDateTime.of(1980, 1, 1, 0, 0)

  def named(name: String): ZipEntry =
    val entry = ZipEntry(name)
    entry.setTimeLocal(EntryTime)
    entry
