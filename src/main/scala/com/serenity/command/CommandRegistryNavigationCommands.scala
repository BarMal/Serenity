package com.serenity.command

/** Comment, bookmark, document-navigation, and LSP commands. Split out of `CommandRegistry.defaultCommands` to keep
  * both under the architecture size targets -- see that method's doc.
  */
private[command] object CommandRegistryNavigationCommands:

  private[command] def commentAndBookmarkCommands: List[Command] = List(
Command.typed(
  "comment-lens",
  "Show or hide the rendered comment at the cursor.",
  CommandIntent.Comments(CommentsIntent.ToggleCommentLens),
  CommandCategory.View,
  label = "Comment Lens"
),
Command.typed(
  "add-document-comment",
  "Add a document comment at the current cursor or selection.",
  CommandIntent.Comments(CommentsIntent.AddDocumentComment("Comment")),
  CommandCategory.Edit,
  label = "Add Document Comment"
),
Command.typed(
  "delete-document-comment",
  "Delete the document comment at the current cursor.",
  CommandIntent.Comments(CommentsIntent.DeleteDocumentComment),
  CommandCategory.Edit,
  label = "Delete Document Comment"
),
Command.typed(
  "goto-line",
  "Go to a specific line number.",
  CommandIntent.Navigation(NavigationIntent.OpenGotoLine),
  CommandCategory.Edit,
  label = "Go to Line"
),
Command.typed(
  "toggle-bookmark",
  "Add or remove a bookmark at the current cursor.",
  CommandIntent.Navigation(NavigationIntent.ToggleBookmark),
  CommandCategory.View,
  label = "Toggle Bookmark"
),
Command.typed(
  "next-bookmark",
  "Go to the next bookmark.",
  CommandIntent.Navigation(NavigationIntent.NextBookmark),
  CommandCategory.View,
  label = "Next Bookmark"
),
Command.typed(
  "previous-bookmark",
  "Go to the previous bookmark.",
  CommandIntent.Navigation(NavigationIntent.PreviousBookmark),
  CommandCategory.View,
  label = "Previous Bookmark"
),
Command.typed(
  "next-document-comment",
  "Go to the next document comment.",
  CommandIntent.Comments(CommentsIntent.NextDocumentComment),
  CommandCategory.View,
  label = "Next Document Comment"
),
  )

  private[command] def navigationAndLspCommands: List[Command] = List(
Command.typed(
  "previous-document-comment",
  "Go to the previous document comment.",
  CommandIntent.Comments(CommentsIntent.PreviousDocumentComment),
  CommandCategory.View,
  label = "Previous Document Comment"
),
Command.typed(
  "navigate-back",
  "Go back to the previous document navigation point.",
  CommandIntent.Navigation(NavigationIntent.NavigateBack),
  CommandCategory.View,
  label = "Navigate Back"
),
Command.typed(
  "navigate-forward",
  "Go forward to the next document navigation point.",
  CommandIntent.Navigation(NavigationIntent.NavigateForward),
  CommandCategory.View,
  label = "Navigate Forward"
),
Command.typed(
  "next-document-symbol",
  "Go to the next document symbol.",
  CommandIntent.Navigation(NavigationIntent.NextDocumentSymbol),
  CommandCategory.View,
  label = "Next Document Symbol"
),
Command.typed(
  "previous-document-symbol",
  "Go to the previous document symbol.",
  CommandIntent.Navigation(NavigationIntent.PreviousDocumentSymbol),
  CommandCategory.View,
  label = "Previous Document Symbol"
),
Command.typed(
  "lsp-hover",
  "Show language-server hover information at the cursor.",
  CommandIntent.Lsp(LspIntent.RequestLspHover),
  CommandCategory.Edit,
  label = "LSP Hover"
),
Command.typed(
  "lsp-completion",
  "Request language-server completion candidates at the cursor.",
  CommandIntent.Lsp(LspIntent.RequestLspCompletion),
  CommandCategory.Edit,
  label = "LSP Completion"
),
Command.typed(
  "lsp-definition",
  "Request the symbol definition from the language server.",
  CommandIntent.Lsp(LspIntent.RequestLspDefinition),
  CommandCategory.Edit,
  label = "LSP Definition"
),
  )

