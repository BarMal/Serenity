package com.serenity.state.components

import com.serenity.state.models.{AppState, Focus}

enum ComponentResult:
  case StateChange(update: AppState => AppState)
  case FocusTransfer(newFocus: Focus)
  case NoChange
  case Composite(results: List[ComponentResult])
  case Dismiss

object ComponentResult:
  def updateState(f: AppState => AppState): ComponentResult = StateChange(f)
  def transferFocus(focus: Focus): ComponentResult          = FocusTransfer(focus)
  def dismiss: ComponentResult                              = Dismiss
  def noChange: ComponentResult                             = NoChange
  def composite(results: ComponentResult*): ComponentResult = Composite(results.toList)
