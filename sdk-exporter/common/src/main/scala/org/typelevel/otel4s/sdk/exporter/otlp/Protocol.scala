package org.typelevel.otel4s.sdk.exporter.otlp

private[exporter] sealed trait Protocol
private[exporter] object Protocol {
  final case class Http(encoding: HttpPayloadEncoding) extends Protocol
}
