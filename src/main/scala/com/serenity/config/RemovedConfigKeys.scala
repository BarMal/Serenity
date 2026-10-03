package com.serenity.config

/** Settings Serenity no longer has. A config file that still names one loads normally; the key is reported once as
  * removed and otherwise ignored, rather than as an unknown key the user might think they mistyped.
  */
object RemovedConfigKeys:

  private val keys: Set[String] = Set(
    "ui.post_processing"
  )

  def isRemoved(key: String): Boolean = keys.contains(key)
