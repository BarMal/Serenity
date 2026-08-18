package com.serenity.state.models

final case class ThemePickerState(
    themes: List[String],
    selectedIndex: Int,
    originalTheme: String
):

  def moveSelection(delta: Int): ThemePickerState =
    if themes.isEmpty then this
    else
      val rawIndex     = (selectedIndex + delta) % themes.length
      val wrappedIndex = if rawIndex < 0 then themes.length + rawIndex else rawIndex
      copy(selectedIndex = wrappedIndex)

  def selectedTheme: Option[String] = themes.lift(selectedIndex)
