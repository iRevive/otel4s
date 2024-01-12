package org.typelevel.otel4s.sdk.metrics.internal

sealed trait InstrumentDescriptor {

  def name: String
  def description: Option[String]
  def unit: Option[String]
  def instrumentType: InstrumentType
  def valueType: InstrumentValueType
  def advice: Advice
}

object InstrumentDescriptor {

  def apply(
      name: String,
      description: Option[String],
      unit: Option[String],
      instrumentType: InstrumentType,
      instrumentValueType: InstrumentValueType,
      advice: Advice
  ): InstrumentDescriptor =
    Impl(name, description, unit, instrumentType, instrumentValueType, advice)

  private final case class Impl(
      name: String,
      description: Option[String],
      unit: Option[String],
      instrumentType: InstrumentType,
      valueType: InstrumentValueType,
      advice: Advice
  ) extends InstrumentDescriptor

}

sealed trait InstrumentType
object InstrumentType {

  case object Counter extends InstrumentType
  case object UpDownCounter extends InstrumentType
  case object Histogram extends InstrumentType
  case object ObservableCounter extends InstrumentType
  case object ObservableUpDownCounter extends InstrumentType
  case object ObservableGauge extends InstrumentType

  val values: List[InstrumentType] = List(
    Counter,
    UpDownCounter,
    Histogram,
    ObservableCounter,
    ObservableUpDownCounter,
    ObservableGauge
  )
}

sealed trait InstrumentValueType
object InstrumentValueType {
  case object Long extends InstrumentValueType
  case object Double extends InstrumentValueType
}

sealed trait Advice
object Advice {
  def empty: Advice = new Advice {}
}
