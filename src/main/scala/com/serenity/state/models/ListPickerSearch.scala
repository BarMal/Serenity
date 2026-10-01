package com.serenity.state.models

import java.util.Locale

import scala.annotation.unused

/** How a [[ListPicker]]'s query turns into its items: the one place a new [[PickerSource]] case gets its meaning. */
object ListPickerSearch:

  /** The picker's items recomputed from its source for its current query, the first one highlighted. A picker without a
    * source keeps the items it has.
    */
  // `state` is for sources computed from the rest of the app, such as a search over open buffers' text.
  def refreshed(picker: ListPicker, @unused state: AppState): ListPicker =
    picker.source.fold(picker) {
      case PickerSource.Fixed(choices) =>
        picker.withChoices(containing(choices, picker.queryText), ListPicker.NoMatches)
    }

  /** A plain case-insensitive substring match, in source order: enough for short lists such as themes, where ranking
    * would reorder choices the user already knows the order of.
    */
  private def containing(choices: Vector[ListChoice], query: String): Vector[ListChoice] =
    val needle = query.toLowerCase(Locale.ROOT)
    choices.filter(choice => (choice.label +: choice.detail.toList).exists(_.toLowerCase(Locale.ROOT).contains(needle)))
