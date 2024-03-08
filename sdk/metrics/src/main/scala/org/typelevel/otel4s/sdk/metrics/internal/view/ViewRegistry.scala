/*
 * Copyright 2024 Typelevel
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.typelevel.otel4s.sdk.metrics.internal.view

import org.typelevel.otel4s.sdk.common.InstrumentationScope
import org.typelevel.otel4s.sdk.metrics.CardinalityLimitSelector
import org.typelevel.otel4s.sdk.metrics.InstrumentSelector
import org.typelevel.otel4s.sdk.metrics.InstrumentType
import org.typelevel.otel4s.sdk.metrics.View
import org.typelevel.otel4s.sdk.metrics.exporter.DefaultAggregationSelector
import org.typelevel.otel4s.sdk.metrics.internal.AttributesProcessor
import org.typelevel.otel4s.sdk.metrics.internal.InstrumentDescriptor
import org.typelevel.otel4s.sdk.metrics.internal.view

import java.util.regex.Pattern

trait ViewRegistry {
  def findViews(
      descriptor: InstrumentDescriptor,
      scope: InstrumentationScope
  ): Vector[RegisteredView]
}

object ViewRegistry {
  def apply(
      defaultAggregationSelector: DefaultAggregationSelector,
      cardinalityLimitSelector: CardinalityLimitSelector,
      registeredViews: Vector[RegisteredView]
  ): ViewRegistry = {
    val defaultViews = InstrumentType.values.map { tpe =>
      tpe -> view.RegisteredView(
        InstrumentSelector.builder.withInstrumentName("*").build,
        View.builder
          .withAggregation(defaultAggregationSelector.select(tpe))
          .build,
        AttributesProcessor.noop,
        cardinalityLimitSelector.select(tpe)
      )
    }

    new Impl(defaultViews.toMap, registeredViews)
  }

  private final class Impl(
      defaultViews: Map[InstrumentType, RegisteredView],
      registeredViews: Vector[RegisteredView]
  ) extends ViewRegistry {
    def findViews(
        descriptor: InstrumentDescriptor,
        scope: InstrumentationScope
    ): Vector[RegisteredView] = {
      val result = registeredViews.filter { entry =>
        matchesSelector(entry.selector, descriptor, scope) &&
        entry.view.aggregation.compatibleWith(descriptor.instrumentType)
      }

      if (result.nonEmpty) {
        result
      } else {
        val defaultView = defaultViews(descriptor.instrumentType)
        // val aggregation = defaultView.view.aggregation

        // todo  if (!aggregation.compatibleWith(descriptor))
        // todo: applyAdviceToDefaultAttribute
        // descriptor.advice.h
        Vector(defaultView)
      }
    }

    private def matchesSelector(
        selector: InstrumentSelector,
        descriptor: InstrumentDescriptor,
        meterScope: InstrumentationScope
    ): Boolean = {
      selector.instrumentType.forall(tpe => descriptor.instrumentType == tpe) &&
      selector.instrumentUnit.forall(unit => descriptor.unit.contains(unit)) &&
      selector.instrumentName.forall(n => toGlobPattern(n)(descriptor.name)) &&
      selector.meterName.forall(n => n == meterScope.name) &&
      selector.meterVersion.forall(v => meterScope.version.contains(v)) &&
      selector.meterSchemaUrl.forall(s => meterScope.schemaUrl.contains(s))
    }

    private def toGlobPattern(globPattern: String): String => Boolean =
      if (globPattern == "*") { _ =>
        true
      } else if (globPattern.exists(c => c == '*' || c == '?')) {
        val pattern = toRegexPattern(globPattern)
        s => pattern.matcher(s).matches()
      } else { s =>
        globPattern.equalsIgnoreCase(s)
      }

    private def toRegexPattern(str: String): Pattern = ???
  }
}
