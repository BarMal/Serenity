package com.serenity.state.models

/** Where a blocking [[ModalDialog]] is placed -- kept distinct from *whether* it blocks, so placement can grow a
  * cursor-adjacent case later without conflating it with modality. Only `Centered` exists today: both current blocking
  * dialogs (`CloseWorkflow`, `FileWorkflow`) are centered, and no dialog needs cursor-adjacent placement yet.
  */
enum ModalPlacement:
  case Centered

/** A blocking dialog on `Runtime.modalStack` -- the explicit modal layer, structurally outside `uiSurfaces`. Only
  * `Modal` cases `ModalStateReducer.isBlocking` classifies as blocking ever live here; every other `Modal` case stays a
  * modeless `SurfaceContent.ModalWorkflow` on the ordinary floating layer, unaffected by this type.
  */
final case class ModalDialog(id: SurfaceId, modal: Modal, placement: ModalPlacement)
