package com.serenity.state.models

enum Focus:
  case EditorPane(paneId: PaneId)
  case Surface(surfaceId: SurfaceId)

enum ModalType:
  case GotoLine
  case Find
  case FileWorkflow
  case ReplaceWorkflow
  case CloseWorkflow
  case Custom(name: String)
