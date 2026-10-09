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
      "resolve-document-comment",
      "Resolve the document comment at the current cursor, hiding it until resolved comments are shown.",
      CommandIntent.Comments(CommentsIntent.ResolveDocumentComment),
      CommandCategory.Edit,
      label = "Resolve Document Comment"
    ),
    Command.typed(
      "reopen-document-comment",
      "Reopen the resolved document comment at the current cursor.",
      CommandIntent.Comments(CommentsIntent.ReopenDocumentComment),
      CommandCategory.Edit,
      label = "Reopen Document Comment"
    ),
    Command.typed(
      "show-resolved-comments",
      "Show or hide resolved document comments in the comment lens, highlights and comment navigation.",
      CommandIntent.Comments(CommentsIntent.ToggleResolvedComments),
      CommandCategory.View,
      label = "Show Resolved Comments"
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
    Command.typed(
      "delete-placeholder",
      "Delete the placeholder at the current cursor.",
      CommandIntent.Placeholders(PlaceholderIntent.DeletePlaceholder),
      CommandCategory.Edit,
      label = "Delete Placeholder"
    ),
    Command.typed(
      "next-placeholder",
      "Go to the next placeholder.",
      CommandIntent.Placeholders(PlaceholderIntent.NextPlaceholder),
      CommandCategory.View,
      label = "Next Placeholder"
    ),
    Command.typed(
      "previous-placeholder",
      "Go to the previous placeholder.",
      CommandIntent.Placeholders(PlaceholderIntent.PreviousPlaceholder),
      CommandCategory.View,
      label = "Previous Placeholder"
    )
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
    Command.typed(
      "lsp-references",
      "Find all references to the symbol from the language server.",
      CommandIntent.Lsp(LspIntent.RequestLspReferences),
      CommandCategory.Edit,
      label = "Find References"
    ),
    Command.typed(
      "lsp-rename",
      "Rename the symbol at the cursor via the language server.",
      CommandIntent.Lsp(LspIntent.OpenRenameSymbolPrompt),
      CommandCategory.Edit,
      label = "Rename Symbol"
    ),
    // #1531: adds the word flagged by the spell-check diagnostic at the cursor to the persisted custom-words
    // dictionary, without requiring a trip through Settings to hand-edit the comma-separated list.
    Command.typed(
      "add-word-to-dictionary",
      "Add the misspelled word at the cursor to the custom spell-check dictionary.",
      CommandIntent.Settings(SettingsIntent.SpellCheck(SpellCheckIntent.AddWordAtCursorToDictionary)),
      CommandCategory.Edit,
      label = "Add Word to Dictionary"
    ),
    Command.typed(
      "show-spelling-suggestions",
      "Suggest corrections for the misspelled word at the cursor.",
      CommandIntent.Spelling(SpellingIntent.ShowSuggestions),
      CommandCategory.Edit,
      label = "Show Spelling Suggestions"
    ),
    Command.typed(
      "ignore-misspelled-word-once",
      "Leave the misspelled word at the cursor alone here, for this session.",
      CommandIntent.Spelling(SpellingIntent.IgnoreOnceAtCursor),
      CommandCategory.Edit,
      label = "Ignore Misspelling Once"
    ),
    Command.typed(
      "ignore-misspelled-word",
      "Leave the misspelled word at the cursor alone everywhere, for this session.",
      CommandIntent.Spelling(SpellingIntent.IgnoreEverywhereAtCursor),
      CommandCategory.Edit,
      label = "Ignore Misspelling Everywhere"
    )
  )
