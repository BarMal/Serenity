package com.serenity.config

object CommentAuthor:

  private val Fallback = "Author"

  /** The name a comment is written under when the config names none. */
  def osUserName: String =
    Option(System.getProperty("user.name")).map(_.trim).filter(_.nonEmpty).getOrElse(Fallback)
