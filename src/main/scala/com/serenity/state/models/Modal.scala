package com.serenity.state.models

import com.serenity.io.{DocumentFormat, FileType}
import com.serenity.text.TextEditing

final case class FileWorkflowSuggestion(
    value: String,
    isDirectory: Boolean = false
)

enum FileWorkflowMode:
  case Open
  case SaveAs

enum FileWorkflowField:
  case Filename
  case Path

enum ReplaceWorkflowField:
  case Find
  case ReplaceWith

enum ReplaceWorkflowAction:
  case ReplaceNext
  case ReplaceAll

enum ReplaceWorkflowScope:
  case CurrentBuffer
  case Selection

  def resolve(
    selection: Option[Selection],
    offsetFor: CursorPosition => Int
  ): Either[ReplaceScopeError, ReplaceScopeRange] =
    this match
      case ReplaceWorkflowScope.CurrentBuffer => Right(ReplaceScopeRange.WholeBuffer)
      case ReplaceWorkflowScope.Selection =>
        selection
          .toRight(ReplaceScopeError.MissingSelection)
          .map(selected => ReplaceScopeRange.Selection(offsetFor(selected.start), offsetFor(selected.end)))

enum ReplaceScopeRange:
  case WholeBuffer
  case Selection(startOffset: Int, endOffset: Int)

enum ReplaceScopeError:
  case MissingSelection

  def message: String =
    this match
      case MissingSelection => "Select text to limit replacement"

enum CloseScope:
  case Current
  case All
  case Others
  case Quit

enum CloseWorkflowChoice:
  case Save
  case Discard
  case Cancel

final case class CloseWorkflowState(
    scope: CloseScope,
    currentBufferId: BufferId,
    currentBufferLabel: String,
    remainingBufferIds: List[BufferId] = Nil,
    selectedChoice: CloseWorkflowChoice = CloseWorkflowChoice.Save
):

  def moveChoice(delta: Int): CloseWorkflowState =
    val choices = List(
      CloseWorkflowChoice.Save,
      CloseWorkflowChoice.Discard,
      CloseWorkflowChoice.Cancel
    )
    val currentIndex = choices.indexOf(selectedChoice)
    val rawIndex     = (currentIndex + delta) % choices.length
    val wrappedIndex = if rawIndex < 0 then choices.length + rawIndex else rawIndex
    copy(selectedChoice = choices(wrappedIndex))

final case class ReplaceWorkflowState(
    findText: String = "",
    replacementText: String = "",
    activeField: ReplaceWorkflowField = ReplaceWorkflowField.Find,
    selectedAction: ReplaceWorkflowAction = ReplaceWorkflowAction.ReplaceAll,
    selectedScope: ReplaceWorkflowScope = ReplaceWorkflowScope.CurrentBuffer,
    statusMessage: Option[String] = None
):

  def appendToActiveField(char: Char): ReplaceWorkflowState =
    activeField match
      case ReplaceWorkflowField.Find        => copy(findText = findText + char, statusMessage = None)
      case ReplaceWorkflowField.ReplaceWith => copy(replacementText = replacementText + char, statusMessage = None)

  def deleteFromActiveField: ReplaceWorkflowState =
    activeField match
      case ReplaceWorkflowField.Find => copy(findText = findText.dropRight(1), statusMessage = None)
      case ReplaceWorkflowField.ReplaceWith =>
        copy(replacementText = replacementText.dropRight(1), statusMessage = None)

  def deleteWordBackwardFromActiveField: ReplaceWorkflowState =
    activeField match
      case ReplaceWorkflowField.Find =>
        copy(findText = TextEditing.deleteWordBackward(findText), statusMessage = None)
      case ReplaceWorkflowField.ReplaceWith =>
        copy(replacementText = TextEditing.deleteWordBackward(replacementText), statusMessage = None)

  def deleteForwardFromActiveField: ReplaceWorkflowState =
    this

  def deleteWordForwardFromActiveField: ReplaceWorkflowState =
    activeField match
      case ReplaceWorkflowField.Find =>
        copy(findText = TextEditing.deleteWordForward(findText), statusMessage = None)
      case ReplaceWorkflowField.ReplaceWith =>
        copy(replacementText = TextEditing.deleteWordForward(replacementText), statusMessage = None)

  def switchField(delta: Int): ReplaceWorkflowState =
    val fields       = List(ReplaceWorkflowField.Find, ReplaceWorkflowField.ReplaceWith)
    val currentIndex = fields.indexOf(activeField)
    val rawIndex     = (currentIndex + delta) % fields.length
    val wrappedIndex = if rawIndex < 0 then fields.length + rawIndex else rawIndex
    copy(activeField = fields(wrappedIndex), statusMessage = None)

  def moveAction(delta: Int): ReplaceWorkflowState =
    val actions      = List(ReplaceWorkflowAction.ReplaceNext, ReplaceWorkflowAction.ReplaceAll)
    val currentIndex = actions.indexOf(selectedAction)
    val rawIndex     = (currentIndex + delta) % actions.length
    val wrappedIndex = if rawIndex < 0 then actions.length + rawIndex else rawIndex
    copy(selectedAction = actions(wrappedIndex), statusMessage = None)

  def moveScope(delta: Int): ReplaceWorkflowState =
    val scopes       = List(ReplaceWorkflowScope.CurrentBuffer, ReplaceWorkflowScope.Selection)
    val currentIndex = scopes.indexOf(selectedScope)
    val rawIndex     = (currentIndex + delta) % scopes.length
    val wrappedIndex = if rawIndex < 0 then scopes.length + rawIndex else rawIndex
    copy(selectedScope = scopes(wrappedIndex), statusMessage = None)

sealed trait FileWorkflowState:
  def mode: FileWorkflowMode
  def filename: String
  def path: String
  def activeField: FileWorkflowField
  def suggestions: List[FileWorkflowSuggestion]
  def selectedSuggestionIndex: Int
  def missingPathSegments: List[String]
  def confirmCreateDirectories: Boolean
  def statusMessage: Option[String]

  /** Whether the buffer this workflow was opened for currently carries rich formatting (marks, alignment, paragraph
    * roles) -- captured once when the workflow modal opens (`openFileWorkflowModal`), not re-derived live, since the
    * buffer being saved cannot change out from under an open save dialog. `false` for `Open` (nothing is being saved)
    * and for a brand-new/never-imported buffer.
    */
  def bufferHasRichFormatting: Boolean

  /** The format the currently-typed `filename` would save as, detected from its extension exactly like a completed save
    * would (`FileType.fromExtension`). A filename with no extension -- notably a brand-new buffer's first Save As,
    * which has no existing extension to inherit -- defaults to [[FileType.Text]] rather than `Unknown`, per issue
    * #1253: a sensible default display rather than new persisted per-buffer state.
    */
  def detectedFileType: FileType =
    val trimmed = filename.trim
    trimmed.lastIndexOf('.') match
      case dotIndex if dotIndex > 0 && dotIndex < trimmed.length - 1 =>
        FileType.fromExtension(trimmed.substring(dotIndex + 1))
      case _ =>
        FileType.Text

  /** True when saving at the currently-typed extension would discard this buffer's rich formatting -- the proactive
    * counterpart to the reactive `LossyRichTextOverwriteException` catch in `saveBufferEffect` (issue #1253).
    */
  def wouldLoseFormatting: Boolean =
    DocumentFormat.wouldLoseFormatting(bufferHasRichFormatting, detectedFileType)

  protected def rebuild(
    filename: String,
    path: String,
    activeField: FileWorkflowField,
    suggestions: List[FileWorkflowSuggestion],
    selectedSuggestionIndex: Int,
    missingPathSegments: List[String],
    confirmCreateDirectories: Boolean,
    statusMessage: Option[String]
  ): FileWorkflowState

  def operationLabel: String
  def supportsFilenameSuggestions: Boolean

  def updated(
    filename: String = filename,
    path: String = path,
    activeField: FileWorkflowField = activeField,
    suggestions: List[FileWorkflowSuggestion] = suggestions,
    selectedSuggestionIndex: Int = selectedSuggestionIndex,
    missingPathSegments: List[String] = missingPathSegments,
    confirmCreateDirectories: Boolean = confirmCreateDirectories,
    statusMessage: Option[String] = statusMessage
  ): FileWorkflowState =
    rebuild(
      filename = filename,
      path = path,
      activeField = activeField,
      suggestions = suggestions,
      selectedSuggestionIndex = selectedSuggestionIndex,
      missingPathSegments = missingPathSegments,
      confirmCreateDirectories = confirmCreateDirectories,
      statusMessage = statusMessage
    )

  def appendToActiveField(char: Char): FileWorkflowState =
    activeField match
      case FileWorkflowField.Filename => updated(filename = filename + char, statusMessage = None)
      case FileWorkflowField.Path     => updated(path = path + char, statusMessage = None)

  def deleteFromActiveField: FileWorkflowState =
    activeField match
      case FileWorkflowField.Filename => updated(filename = filename.dropRight(1), statusMessage = None)
      case FileWorkflowField.Path     => updated(path = path.dropRight(1), statusMessage = None)

  def deleteWordBackwardFromActiveField: FileWorkflowState =
    activeField match
      case FileWorkflowField.Filename =>
        updated(filename = TextEditing.deleteWordBackward(filename), statusMessage = None)
      case FileWorkflowField.Path =>
        updated(path = TextEditing.deleteWordBackward(path), statusMessage = None)

  def deleteForwardFromActiveField: FileWorkflowState =
    this

  def deleteWordForwardFromActiveField: FileWorkflowState =
    activeField match
      case FileWorkflowField.Filename =>
        updated(filename = TextEditing.deleteWordForward(filename), statusMessage = None)
      case FileWorkflowField.Path =>
        updated(path = TextEditing.deleteWordForward(path), statusMessage = None)

  def switchField(delta: Int): FileWorkflowState =
    val fields       = List(FileWorkflowField.Filename, FileWorkflowField.Path)
    val currentIndex = fields.indexOf(activeField)
    val rawIndex     = (currentIndex + delta) % fields.length
    val wrappedIndex = if rawIndex < 0 then fields.length + rawIndex else rawIndex
    updated(activeField = fields(wrappedIndex), statusMessage = None)

  def moveSuggestion(delta: Int): FileWorkflowState =
    if suggestions.isEmpty then this
    else
      val rawIndex     = (selectedSuggestionIndex + delta) % suggestions.length
      val wrappedIndex = if rawIndex < 0 then suggestions.length + rawIndex else rawIndex
      updated(selectedSuggestionIndex = wrappedIndex, statusMessage = None)

  def applySelectedSuggestion: FileWorkflowState =
    suggestions.lift(selectedSuggestionIndex) match
      case Some(suggestion) =>
        val separator = java.io.File.separator
        val normalizedValue =
          if suggestion.isDirectory && !suggestion.value.endsWith(separator) then suggestion.value + separator
          else suggestion.value
        activeField match
          case FileWorkflowField.Path =>
            updated(path = normalizedValue, statusMessage = None)
          case FileWorkflowField.Filename =>
            updated(filename = normalizedValue, statusMessage = None)
      case None => this

final case class OpenFileWorkflowState(
    filename: String = "",
    path: String = "",
    activeField: FileWorkflowField = FileWorkflowField.Filename,
    suggestions: List[FileWorkflowSuggestion] = Nil,
    selectedSuggestionIndex: Int = 0,
    missingPathSegments: List[String] = Nil,
    confirmCreateDirectories: Boolean = false,
    statusMessage: Option[String] = None,
    bufferHasRichFormatting: Boolean = false
) extends FileWorkflowState:
  val mode: FileWorkflowMode               = FileWorkflowMode.Open
  val operationLabel: String               = "open"
  val supportsFilenameSuggestions: Boolean = true

  protected def rebuild(
    filename: String,
    path: String,
    activeField: FileWorkflowField,
    suggestions: List[FileWorkflowSuggestion],
    selectedSuggestionIndex: Int,
    missingPathSegments: List[String],
    confirmCreateDirectories: Boolean,
    statusMessage: Option[String]
  ): FileWorkflowState =
    copy(
      filename = filename,
      path = path,
      activeField = activeField,
      suggestions = suggestions,
      selectedSuggestionIndex = selectedSuggestionIndex,
      missingPathSegments = missingPathSegments,
      confirmCreateDirectories = confirmCreateDirectories,
      statusMessage = statusMessage
    )

final case class SaveAsFileWorkflowState(
    filename: String = "",
    path: String = "",
    activeField: FileWorkflowField = FileWorkflowField.Filename,
    suggestions: List[FileWorkflowSuggestion] = Nil,
    selectedSuggestionIndex: Int = 0,
    missingPathSegments: List[String] = Nil,
    confirmCreateDirectories: Boolean = false,
    statusMessage: Option[String] = None,
    bufferHasRichFormatting: Boolean = false
) extends FileWorkflowState:
  val mode: FileWorkflowMode               = FileWorkflowMode.SaveAs
  val operationLabel: String               = "save-as"
  val supportsFilenameSuggestions: Boolean = false

  protected def rebuild(
    filename: String,
    path: String,
    activeField: FileWorkflowField,
    suggestions: List[FileWorkflowSuggestion],
    selectedSuggestionIndex: Int,
    missingPathSegments: List[String],
    confirmCreateDirectories: Boolean,
    statusMessage: Option[String]
  ): FileWorkflowState =
    copy(
      filename = filename,
      path = path,
      activeField = activeField,
      suggestions = suggestions,
      selectedSuggestionIndex = selectedSuggestionIndex,
      missingPathSegments = missingPathSegments,
      confirmCreateDirectories = confirmCreateDirectories,
      statusMessage = statusMessage
    )

object FileWorkflowState:

  def apply(
    mode: FileWorkflowMode,
    filename: String = "",
    path: String = "",
    activeField: FileWorkflowField = FileWorkflowField.Filename,
    suggestions: List[FileWorkflowSuggestion] = Nil,
    selectedSuggestionIndex: Int = 0,
    missingPathSegments: List[String] = Nil,
    confirmCreateDirectories: Boolean = false,
    statusMessage: Option[String] = None,
    bufferHasRichFormatting: Boolean = false
  ): FileWorkflowState =
    mode match
      case FileWorkflowMode.Open =>
        OpenFileWorkflowState(
          filename = filename,
          path = path,
          activeField = activeField,
          suggestions = suggestions,
          selectedSuggestionIndex = selectedSuggestionIndex,
          missingPathSegments = missingPathSegments,
          confirmCreateDirectories = confirmCreateDirectories,
          statusMessage = statusMessage,
          bufferHasRichFormatting = bufferHasRichFormatting
        )
      case FileWorkflowMode.SaveAs =>
        SaveAsFileWorkflowState(
          filename = filename,
          path = path,
          activeField = activeField,
          suggestions = suggestions,
          selectedSuggestionIndex = selectedSuggestionIndex,
          missingPathSegments = missingPathSegments,
          confirmCreateDirectories = confirmCreateDirectories,
          statusMessage = statusMessage,
          bufferHasRichFormatting = bufferHasRichFormatting
        )

enum Modal:

  case GotoLine(
      input: String
  )

  case Find(
      query: String,
      results: List[FindResult],
      currentIndex: Int
  )

  case Custom(
      name: String,
      input: String = ""
  )

  case FileWorkflow(
      workflow: FileWorkflowState
  )

  case ReplaceWorkflow(
      workflow: ReplaceWorkflowState
  )

  case CloseWorkflow(
      workflow: CloseWorkflowState
  )
