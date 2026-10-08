package com.serenity.config

extension (trigger: HotkeyTrigger)

  /** The trigger as a person reads it on `osName`: `Cmd+P` on macOS, `Ctrl+P` elsewhere. [[HotkeyTrigger.render]] stays
    * the config-file spelling.
    */
  def label(osName: String): String =
    val onMac     = osName.toLowerCase(java.util.Locale.ROOT).contains("mac")
    val parts     = trigger.render.split("\\+", -1).toList
    val modifiers = if trigger.isBareModifierChord then 1 else trigger.modifiers.size
    (parts.take(modifiers) :+ parts.drop(modifiers).mkString("+"))
      .map(part => HotkeyTriggerLabel.name(part, onMac))
      .mkString("+")

private object HotkeyTriggerLabel:

  private val keyNames: Map[String, String] =
    Map("pageup" -> "PageUp", "pagedown" -> "PageDown", "reverse-tab" -> "ReverseTab", "eof" -> "EOF")

  def name(part: String, onMac: Boolean): String =
    if onMac && part == "meta" then "Cmd"
    else if keyNames.contains(part) then keyNames(part)
    else part.headOption.fold(part)(first => first.toUpper.toString + part.drop(1))
