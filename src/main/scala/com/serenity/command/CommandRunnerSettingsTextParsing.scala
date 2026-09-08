package com.serenity.command

/** Small text-parsing helpers shared across the command-runner settings input item objects: trimming/validating free
  * text and splitting comma lists. Extracted from `CommandRunnerSettingsInputItems` (which grew past its 600-line
  * target) rather than owned by any one input-item category, since more than one sibling depends on these staying in
  * one place instead of reaching back into the object most of them used to be split off of.
  */
private[command] object CommandRunnerSettingsTextParsing:

  def nonEmptyText(text: String): Option[String] =
    Option(text.trim).filter(_.nonEmpty)

  def namedPair(text: String): Option[(String, String)] =
    text.split("->", 2).toList match
      case source :: target :: Nil =>
        for
          normalizedSource <- nonEmptyText(source)
          normalizedTarget <- nonEmptyText(target)
        yield (normalizedSource, normalizedTarget)
      case _ =>
        None

  def nonEmptyCommaList(text: String): Option[List[String]] =
    Option(commaList(text)).filter(_.nonEmpty)

  def commaList(text: String): List[String] =
    text
      .split(",")
      .toList
      .map(_.trim.toLowerCase)
      .filter(_.nonEmpty)
      .distinct

  def commaListPreserveCase(text: String): List[String] =
    text
      .split(",")
      .toList
      .map(_.trim)
      .filter(_.nonEmpty)
      .distinct
