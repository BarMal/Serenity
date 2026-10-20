package com.serenity.state.models

object ImeNotices:

  val refused: Notice =
    Notice(NoticeLevel.Info, "That line is a read-only block, so text cannot be composed on it.")
