package com.serenity.ui.theme

/** Lexical state carried across line boundaries by the token-aware tokenizer, needed because a comment or string can
  * remain open at the end of one line and close partway through a later one.
  */
enum LexState:
  case Default
  case InBlockComment
  case InTripleQuotedString
