package org.typelevel.otel4s.logs

import cats.{Hash, Show}

/** Represents the severity of a log record.
  *
  * @param severity
  *   smaller numerical values correspond to less severe events (such as debug events), larger numerical values
  *   correspond to more severe events (such as errors and critical events)
  *
  * @see
  *   [[https://opentelemetry.io/docs/specs/otel/logs/data-model/#field-severitynumber]]
  */
sealed abstract class Severity(val severity: Int) {

  override final def hashCode(): Int =
    Hash[Severity].hash(this)

  override final def equals(obj: Any): Boolean =
    obj match {
      case other: Severity => Hash[Severity].eqv(this, other)
      case _               => false
    }

  override final def toString: String =
    Show[Severity].show(this)

}

object Severity {

  /** A fine-grained debugging event. Typically disabled in default configurations.
    */
  sealed abstract class Trace(severity: Int) extends Severity(severity)
  object Trace {
    def trace1: Trace = Trace1
    def trace2: Trace = Trace2
    def trace3: Trace = Trace3
    def trace4: Trace = Trace4

    private[otel4s] case object Trace1 extends Trace(1)
    private[otel4s] case object Trace2 extends Trace(2)
    private[otel4s] case object Trace3 extends Trace(3)
    private[otel4s] case object Trace4 extends Trace(4)
  }

  /** A debugging event.
    */
  sealed abstract class Debug(severity: Int) extends Severity(severity)
  object Debug {
    def debug1: Debug = Debug1
    def debug2: Debug = Debug2
    def debug3: Debug = Debug3
    def debug4: Debug = Debug4

    private[otel4s] case object Debug1 extends Debug(5)
    private[otel4s] case object Debug2 extends Debug(6)
    private[otel4s] case object Debug3 extends Debug(7)
    private[otel4s] case object Debug4 extends Debug(8)
  }

  /** An informational event. Indicates that an event happened.
    */
  sealed abstract class Info(severity: Int) extends Severity(severity)
  object Info {
    def info1: Info = Info1
    def info2: Info = Info2
    def info3: Info = Info3
    def info4: Info = Info4

    private[otel4s] case object Info1 extends Info(9)
    private[otel4s] case object Info2 extends Info(10)
    private[otel4s] case object Info3 extends Info(11)
    private[otel4s] case object Info4 extends Info(12)
  }

  /** A warning event. Not an error but is likely more important than an informational event.
    */
  sealed abstract class Warn(severity: Int) extends Severity(severity)
  object Warn {
    def warn1: Warn = Warn1
    def warn2: Warn = Warn2
    def warn3: Warn = Warn3
    def warn4: Warn = Warn4

    private[otel4s] case object Warn1 extends Warn(13)
    private[otel4s] case object Warn2 extends Warn(14)
    private[otel4s] case object Warn3 extends Warn(15)
    private[otel4s] case object Warn4 extends Warn(16)
  }

  /** An error event. Something went wrong.
    */
  sealed abstract class Error(severity: Int) extends Severity(severity)
  object Error {
    def error1: Error = Error1
    def error2: Error = Error2
    def error3: Error = Error3
    def error4: Error = Error4

    private[otel4s] case object Error1 extends Error(17)
    private[otel4s] case object Error2 extends Error(18)
    private[otel4s] case object Error3 extends Error(19)
    private[otel4s] case object Error4 extends Error(20)
  }

  /** A fatal error such as application or system crash.
    */
  sealed abstract class Fatal(severity: Int) extends Severity(severity)
  object Fatal {
    def fatal1: Fatal = Fatal1
    def fatal2: Fatal = Fatal2
    def fatal3: Fatal = Fatal3
    def fatal4: Fatal = Fatal4

    private[otel4s] case object Fatal1 extends Fatal(21)
    private[otel4s] case object Fatal2 extends Fatal(22)
    private[otel4s] case object Fatal3 extends Fatal(23)
    private[otel4s] case object Fatal4 extends Fatal(24)
  }

  /*def fromNumber(number: Int): Option[Severity] =
    values.find(_.severity == number)*/

  implicit val severityHash: Hash[Severity] = Hash.by(_.severity)

  implicit val severityShow: Show[Severity] = Show.fromToString
}
