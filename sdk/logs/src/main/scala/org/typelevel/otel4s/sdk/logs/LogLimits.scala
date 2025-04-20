package org.typelevel.otel4s.sdk.logs

sealed trait LogLimits {
  def maxNumberOfLogs: Int
}

object LogLimits {
  def default: LogLimits = new LogLimits {
    def maxNumberOfLogs: Int = 1

  }
}