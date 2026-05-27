package com.serenity.state.reducers

import com.serenity.state.models.AppState

trait Reducer[-E]:
  def reduce(event: E, currentState: AppState): ReducerResult

object Reducer:
  def instance[E](f: (E, AppState) => ReducerResult): Reducer[E] =
    new Reducer[E]:
      override def reduce(event: E, currentState: AppState): ReducerResult =
        f(event, currentState)
