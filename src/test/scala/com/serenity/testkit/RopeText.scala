package com.serenity.testkit

import com.serenity.rope.{Balance, Rope}

/** A rope over `text` with the default balance, for specs that hand text to APIs which carry it as a rope. */
object RopeText:

  def apply(text: String): Rope = Rope(text)(using Balance.default)
