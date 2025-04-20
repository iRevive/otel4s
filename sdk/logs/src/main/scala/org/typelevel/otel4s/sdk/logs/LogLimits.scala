package org.typelevel.otel4s.sdk.logs

sealed trait LogLimits {

}

object LogLimits {
  def default: LogLimits = new LogLimits {

  }

}
