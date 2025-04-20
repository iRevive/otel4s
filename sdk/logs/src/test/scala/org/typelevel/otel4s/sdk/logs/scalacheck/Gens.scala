/*
 * Copyright 2023 Typelevel
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

package org.typelevel.otel4s.sdk.logs.scalacheck

import org.scalacheck.Gen
// Severity has been moved to org.typelevel.otel4s.logs.Severity

trait Gens extends org.typelevel.otel4s.sdk.scalacheck.Gens {

  // Severity has been moved to org.typelevel.otel4s.logs.Severity
  // Use org.typelevel.otel4s.logs.scalacheck.Gens.severity instead
  // val severity: Gen[Severity] =
  //   Gen.oneOf(Severity.values)

}

object Gens extends Gens
