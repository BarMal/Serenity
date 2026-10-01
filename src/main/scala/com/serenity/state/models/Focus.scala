package com.serenity.state.models

enum Focus:
  case EditorPane(paneId: PaneId)
  case Surface(surfaceId: SurfaceId)
  case Modal

enum ModalType:
  case TextPrompt
  case Find
  case FileWorkflow
  case ReplaceWorkflow
  case CloseWorkflow
  case Confirm
  case ListPicker
  case Custom(name: String)
