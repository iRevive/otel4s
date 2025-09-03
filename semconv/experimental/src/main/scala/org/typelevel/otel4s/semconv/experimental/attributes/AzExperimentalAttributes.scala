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

package org.typelevel.otel4s
package semconv
package experimental.attributes

// DO NOT EDIT, this is an Auto-generated file from buildscripts/templates/registry/otel4s/attributes/SemanticAttributes.scala.j2
object AzExperimentalAttributes {

  /** Deprecated, use `azure.resource_provider.namespace` instead.
    */
  @deprecated("Replaced by `azure.resource_provider.namespace`.", "")
  val AzNamespace: AttributeKey[String] =
    AttributeKey("az.namespace")

  /** Deprecated, use `azure.service.request.id` instead.
    */
  @deprecated("Replaced by `azure.service.request.id`.", "")
  val AzServiceRequestId: AttributeKey[String] =
    AttributeKey("az.service_request_id")

}
