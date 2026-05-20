package com.serenity.state.models

import com.serenity.ui.layout.PanelPosition

enum Focus:
  case EditorPane(paneId: PaneId)
  case PinnedPanel(position: PanelPosition)
  case PeekOverlay
  case Modal(modalType: ModalType)

enum ModalType:
  case CommandPalette
  case FileSearch
  case QuickOpen
  case GotoLine
  case Custom(name: String)
