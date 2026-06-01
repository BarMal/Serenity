package com.serenity.keystroke.events

trait Event

trait AppEvent       extends Event
trait GlobalAppEvent extends AppEvent

trait EditorEvent extends Event

trait SystemEvent extends Event
