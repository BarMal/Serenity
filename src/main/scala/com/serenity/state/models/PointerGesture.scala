package com.serenity.state.models

/** What a primary-button press began, so the drag that follows continues that gesture and nothing else -- a text drag
  * that wanders over a margin or dock must not start resizing it.
  */
enum PointerGesture:
  case PanelResize
  case TextAreaResize
  case TabReorder
  case TextSelection
  case Other
