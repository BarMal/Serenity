package com.serenity.state.models

import java.util.Locale

import com.serenity.ui.widget.Loadable

/** How a [[ListPicker]]'s query turns into its items: the one place a new [[PickerSource]] case gets its meaning. */
object ListPickerSearch:

  /** The picker's items recomputed from its source for its current query, the first one highlighted. A picker without a
    * source keeps the items it has.
    */
  def refreshed(picker: ListPicker, state: AppState): ListPicker =
    picker.source.fold(picker) {
      case PickerSource.Fixed(choices) =>
        picker.withChoices(containing(choices, picker.queryText), ListPicker.NoMatches)
      case source: PickerSource.BufferText =>
        searchedBufferText(picker, source, state)
    }

  /** The picker with its source's next batch of choices appended to its items, the highlight left where it is;
    * unchanged when its source has no more.
    */
  def extended(picker: ListPicker, state: AppState): ListPicker =
    (picker.source, picker.items) match
      case (Some(source @ PickerSource.BufferText(batchSize, Some(from))), Loadable.Ready(choices)) =>
        val (more, resumeAt) = BufferTextSearch.batch(state, picker.queryText, batchSize, Some(from))
        picker.copy(
          items = Loadable.Ready(choices.copy(items = choices.items ++ more)),
          source = Some(source.copy(resumeAt = resumeAt))
        )
      case _ => picker

  /** A plain case-insensitive substring match, in source order: enough for short lists such as themes, where ranking
    * would reorder choices the user already knows the order of.
    */
  private def containing(choices: Vector[ListChoice], query: String): Vector[ListChoice] =
    val needle = query.toLowerCase(Locale.ROOT)
    choices.filter(choice => (choice.label +: choice.detail.toList).exists(_.toLowerCase(Locale.ROOT).contains(needle)))

  // An empty query would match every line of every buffer, which is noise rather than a result.
  private def searchedBufferText(picker: ListPicker, source: PickerSource.BufferText, state: AppState): ListPicker =
    if picker.queryText.isEmpty then
      picker.copy(
        items = Loadable.Empty(BufferTextSearch.EmptyQueryMessage),
        source = Some(source.copy(resumeAt = None))
      )
    else
      val (choices, resumeAt) = BufferTextSearch.batch(state, picker.queryText, source.batchSize, None)
      picker.withChoices(choices, ListPicker.NoMatches).copy(source = Some(source.copy(resumeAt = resumeAt)))
