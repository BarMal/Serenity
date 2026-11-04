package com.serenity.state.models

/** The tab an explorer preview opened, and the version of its content then. Any edit raises the content version, so a
  * tab stops being a preview the moment it is edited, however the text ends up.
  */
final case class PreviewTab(bufferId: BufferId, contentVersion: Long)
