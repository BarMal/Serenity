package com.serenity.state.models

case class FileWorkflowSuggestion(
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

enum CloseScope:
  case Current
  case All
  case Others
  case Quit

enum CloseWorkflowChoice:
  case Save
  case Discard
  case Cancel

case class CloseWorkflowState(
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
    val rawIndex = (currentIndex + delta) % choices.length
    val wrappedIndex = if rawIndex < 0 then choices.length + rawIndex else rawIndex
    copy(selectedChoice = choices(wrappedIndex))

case class ReplaceWorkflowState(
    findText: String = "",
    replacementText: String = "",
    activeField: ReplaceWorkflowField = ReplaceWorkflowField.Find,
    statusMessage: Option[String] = None
):
  def appendToActiveField(char: Char): ReplaceWorkflowState =
    activeField match
      case ReplaceWorkflowField.Find        => copy(findText = findText + char, statusMessage = None)
      case ReplaceWorkflowField.ReplaceWith => copy(replacementText = replacementText + char, statusMessage = None)

  def deleteFromActiveField: ReplaceWorkflowState =
    activeField match
      case ReplaceWorkflowField.Find        => copy(findText = findText.dropRight(1), statusMessage = None)
      case ReplaceWorkflowField.ReplaceWith => copy(replacementText = replacementText.dropRight(1), statusMessage = None)

  def switchField(delta: Int): ReplaceWorkflowState =
    val fields = List(ReplaceWorkflowField.Find, ReplaceWorkflowField.ReplaceWith)
    val currentIndex = fields.indexOf(activeField)
    val rawIndex = (currentIndex + delta) % fields.length
    val wrappedIndex = if rawIndex < 0 then fields.length + rawIndex else rawIndex
    copy(activeField = fields(wrappedIndex), statusMessage = None)

case class FileWorkflowState(
    mode: FileWorkflowMode,
    filename: String = "",
    path: String = "",
    activeField: FileWorkflowField = FileWorkflowField.Filename,
    suggestions: List[FileWorkflowSuggestion] = Nil,
    selectedSuggestionIndex: Int = 0,
    missingPathSegments: List[String] = Nil,
    confirmCreateDirectories: Boolean = false,
    statusMessage: Option[String] = None
):
  def appendToActiveField(char: Char): FileWorkflowState =
    activeField match
      case FileWorkflowField.Filename => copy(filename = filename + char, statusMessage = None)
      case FileWorkflowField.Path     => copy(path = path + char, statusMessage = None)

  def deleteFromActiveField: FileWorkflowState =
    activeField match
      case FileWorkflowField.Filename => copy(filename = filename.dropRight(1), statusMessage = None)
      case FileWorkflowField.Path     => copy(path = path.dropRight(1), statusMessage = None)

  def switchField(delta: Int): FileWorkflowState =
    val fields = List(FileWorkflowField.Filename, FileWorkflowField.Path)
    val currentIndex = fields.indexOf(activeField)
    val rawIndex = (currentIndex + delta) % fields.length
    val wrappedIndex = if rawIndex < 0 then fields.length + rawIndex else rawIndex
    copy(activeField = fields(wrappedIndex), statusMessage = None)

  def moveSuggestion(delta: Int): FileWorkflowState =
    if suggestions.isEmpty then this
    else
      val rawIndex = (selectedSuggestionIndex + delta) % suggestions.length
      val wrappedIndex = if rawIndex < 0 then suggestions.length + rawIndex else rawIndex
      copy(selectedSuggestionIndex = wrappedIndex, statusMessage = None)

  def applySelectedSuggestion: FileWorkflowState =
    suggestions.lift(selectedSuggestionIndex) match
      case Some(suggestion) =>
        val separator = java.io.File.separator
        val normalizedValue =
          if suggestion.isDirectory && !suggestion.value.endsWith(separator) then suggestion.value + separator
          else suggestion.value
        copy(path = normalizedValue, statusMessage = None)
      case None             => this

enum Modal:

  case GotoLine(
      input: String
  )

  case Find(
      query: String,
      resultLines: List[Int],
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
