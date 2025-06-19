/*
 * Copyright 2025 Typelevel
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

import org.scalacheck.Cogen
// Severity has been moved to org.typelevel.otel4s.logs.Severity

trait Cogens extends org.typelevel.otel4s.sdk.scalacheck.Cogens {

  // Severity has been moved to org.typelevel.otel4s.logs.Severity
  // Use org.typelevel.otel4s.logs.scalacheck.Cogens.severityCogen instead
  // implicit val severityCogen: Cogen[Severity] =
  //   Cogen[Int].contramap(_.number)

}

object Cogens extends Cogens
