package com.serenity.ui.layout

import io.circe.*
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}

given Encoder[PanelPosition] = Encoder.encodeString.contramap(_.toString)

given Decoder[PanelPosition] = Decoder.decodeString.emap {
  case "Left"   => Right(PanelPosition.Left)
  case "Right"  => Right(PanelPosition.Right)
  case "Bottom" => Right(PanelPosition.Bottom)
  case "Top"    => Right(PanelPosition.Top)
  case other    => Left(s"Unknown PanelPosition: $other")
}

given Encoder[SymbolKind] = Encoder.encodeString.contramap(_.toString)

given Decoder[SymbolKind] = Decoder.decodeString.emap {
  case "Function" => Right(SymbolKind.Function)
  case "Class"    => Right(SymbolKind.Class)
  case "Method"   => Right(SymbolKind.Method)
  case "Variable" => Right(SymbolKind.Variable)
  case "Constant" => Right(SymbolKind.Constant)
  case "Heading"  => Right(SymbolKind.Heading)
  case "Bookmark" => Right(SymbolKind.Bookmark)
  case "Section"  => Right(SymbolKind.Section)
  case other      => Left(s"Unknown SymbolKind: $other")
}

given Encoder[DiagnosticSeverity] = Encoder.encodeString.contramap(_.toString)

given Decoder[DiagnosticSeverity] = Decoder.decodeString.emap {
  case "Error"   => Right(DiagnosticSeverity.Error)
  case "Warning" => Right(DiagnosticSeverity.Warning)
  case "Info"    => Right(DiagnosticSeverity.Info)
  case "Hint"    => Right(DiagnosticSeverity.Hint)
  case other     => Left(s"Unknown DiagnosticSeverity: $other")
}

given Encoder[Location] = deriveEncoder
given Decoder[Location] = deriveDecoder

given Encoder[Symbol] = deriveEncoder
given Decoder[Symbol] = deriveDecoder

given Encoder[Diagnostic] = deriveEncoder
given Decoder[Diagnostic] = deriveDecoder

given Encoder[SessionPanelContent] = deriveEncoder
given Decoder[SessionPanelContent] = deriveDecoder

given Encoder[SessionPinnedPanel] = deriveEncoder
given Decoder[SessionPinnedPanel] = deriveDecoder

given Encoder[SessionWorkspaceNode] = deriveEncoder
given Decoder[SessionWorkspaceNode] = deriveDecoder

given Encoder[SessionDockedPanel] = deriveEncoder
given Decoder[SessionDockedPanel] = deriveDecoder
