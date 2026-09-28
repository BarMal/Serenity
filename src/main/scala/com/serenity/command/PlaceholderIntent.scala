package com.serenity.command

enum PlaceholderIntent:
  case AddPlaceholder(note: String)
  case DeletePlaceholder
  case NextPlaceholder
  case PreviousPlaceholder
