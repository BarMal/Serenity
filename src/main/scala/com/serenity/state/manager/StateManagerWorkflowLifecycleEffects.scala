package com.serenity.state.manager

import cats.effect.IO
import com.serenity.state.models.*
import com.serenity.state.reducers.*

/** Workflow operations selected by command effects. */
private[manager] trait WorkflowEffectPort:
  def requestOpenFile: IO[Unit]
  def requestSaveAs: IO[Unit]
  def refresh(surfaceId: SurfaceId): IO[Unit]
  def refreshFind(request: FindSearchRequest): IO[Unit]
  def submitFile(surfaceId: SurfaceId): IO[Unit]
  def openAsProjectRoot(surfaceId: SurfaceId): IO[Unit]
  def submitReplace(surfaceId: SurfaceId): IO[Unit]
  def beginClose(scope: CloseScope): IO[Unit]
  def submitClose(surfaceId: SurfaceId): IO[Unit]
  def submitReloadConflict(surfaceId: SurfaceId): IO[Unit]
  def createDirectories(surfaceId: SurfaceId): IO[Unit]
  def submitSessionNamePrompt(surfaceId: SurfaceId): IO[Unit]
  def submitSessionList(surfaceId: SurfaceId): IO[Unit]

/** Interprets workflow effects without editor, theme, file, or runtime dependencies. */
final private[manager] class WorkflowEffectHandler(port: WorkflowEffectPort):

  def interpret(effect: WorkflowEffect): IO[Unit] =
    effect match
      case WorkflowEffect.RequestOpenFile                   => port.requestOpenFile
      case WorkflowEffect.RequestSaveAs                     => port.requestSaveAs
      case WorkflowEffect.RefreshFileWorkflow(id)           => port.refresh(id)
      case WorkflowEffect.RefreshFind(request)              => port.refreshFind(request)
      case WorkflowEffect.SubmitFileWorkflow(id)            => port.submitFile(id)
      case WorkflowEffect.OpenFileWorkflowAsProjectRoot(id) => port.openAsProjectRoot(id)
      case WorkflowEffect.SubmitReplaceWorkflow(id)         => port.submitReplace(id)
      case WorkflowEffect.BeginClose(scope)                 => port.beginClose(scope)
      case WorkflowEffect.SubmitCloseWorkflow(id)           => port.submitClose(id)
      case WorkflowEffect.SubmitReloadConflict(id)          => port.submitReloadConflict(id)
      case WorkflowEffect.CreateFileWorkflowDirectories(id) => port.createDirectories(id)
      case WorkflowEffect.SubmitSessionNamePrompt(id)       => port.submitSessionNamePrompt(id)
      case WorkflowEffect.SubmitSessionList(id)             => port.submitSessionList(id)

/** Lifecycle operation required by lifecycle effects. */
private[manager] trait LifecycleEffectPort:
  def completeQuit: IO[Unit]

/** Interprets lifecycle effects without runtime, editor, or workflow dependencies. */
final private[manager] class LifecycleEffectHandler(port: LifecycleEffectPort):

  def interpret: IO[Unit] = port.completeQuit
