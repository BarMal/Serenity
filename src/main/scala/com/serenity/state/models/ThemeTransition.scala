package com.serenity.state.models

import com.serenity.ui.theme.Theme

final case class ThemeTransition(previousTheme: Theme, currentStep: Int, totalSteps: Int):
  def progress: Double         = if totalSteps <= 0 then 1.0 else currentStep.toDouble / totalSteps
  def advance: ThemeTransition = copy(currentStep = currentStep + 1)
  def isComplete: Boolean      = currentStep >= totalSteps
