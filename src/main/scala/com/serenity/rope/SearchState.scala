package com.serenity.rope

enum SearchState:
  case Found(index: Int)
  case Poll
  case PollAndPrune
