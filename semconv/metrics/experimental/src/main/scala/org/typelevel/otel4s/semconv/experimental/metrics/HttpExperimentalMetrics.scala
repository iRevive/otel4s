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

package org.typelevel.otel4s
package semconv
package experimental
package metrics

import cats.effect.Resource
import org.typelevel.otel4s.metrics._
import org.typelevel.otel4s.semconv.attributes._
import org.typelevel.otel4s.semconv.experimental.attributes._

// DO NOT EDIT, this is an Auto-generated file from buildscripts/templates/registry/otel4s/metrics/SemanticMetrics.scala.j2
object HttpExperimentalMetrics {

  @annotation.nowarn("cat=deprecation")
  val specs: List[MetricSpec] = List(
    ClientActiveRequests,
    ClientConnectionDuration,
    ClientOpenConnections,
    ClientRequestBodySize,
    ClientRequestDuration,
    ClientResponseBodySize,
    ServerActiveRequests,
    ServerRequestBodySize,
    ServerRequestDuration,
    ServerResponseBodySize,
  )

  /** Number of active HTTP requests.
    */
  object ClientActiveRequests extends MetricSpec.Unsealed {

    val name: String = "http.client.active_requests"
    val description: String = "Number of active HTTP requests."
    val unit: String = "{request}"
    val stability: Stability = Stability.development
    val attributeSpecs: List[AttributeSpec[_]] = AttributeSpecs.specs

    object AttributeSpecs {

      /** HTTP request method.
        *
        * @note
        *   <p> HTTP request method value SHOULD be "known" to the instrumentation. By default, this convention defines
        *   "known" methods as the ones listed in <a
        *   href="https://www.rfc-editor.org/rfc/rfc9110.html#name-methods">RFC9110</a> and the PATCH method defined in
        *   <a href="https://www.rfc-editor.org/rfc/rfc5789.html">RFC5789</a>. <p> If the HTTP request method is not
        *   known to instrumentation, it MUST set the `http.request.method` attribute to `_OTHER`. <p> If the HTTP
        *   instrumentation could end up converting valid HTTP request methods to `_OTHER`, then it MUST provide a way
        *   to override the list of known HTTP methods. If this override is done via environment variable, then the
        *   environment variable MUST be named OTEL_INSTRUMENTATION_HTTP_KNOWN_METHODS and support a comma-separated
        *   list of case-sensitive known HTTP methods (this list MUST be a full override of the default known method, it
        *   is not a list of known methods in addition to the defaults). <p> HTTP method names are case-sensitive and
        *   `http.request.method` attribute value MUST match a known HTTP method name exactly. Instrumentations for
        *   specific web frameworks that consider HTTP methods to be case insensitive, SHOULD populate a canonical
        *   equivalent. Tracing instrumentations that do so, MUST also set `http.request.method_original` to the
        *   original value.
        */
      val httpRequestMethod: AttributeSpec[String] =
        AttributeSpec(
          HttpAttributes.HttpRequestMethod,
          List(
            "GET",
            "POST",
            "HEAD",
          ),
          Requirement.recommended,
          Stability.stable
        )

      /** Server domain name if available without reverse DNS lookup; otherwise, IP address or Unix domain socket name.
        *
        * @note
        *   <p> When observed from the client side, and when communicating through an intermediary, `server.address`
        *   SHOULD represent the server address behind any intermediaries, for example proxies, if it's available.
        */
      val serverAddress: AttributeSpec[String] =
        AttributeSpec(
          ServerAttributes.ServerAddress,
          List(
            "example.com",
            "10.1.2.80",
            "/tmp/my.sock",
          ),
          Requirement.required,
          Stability.stable
        )

      /** Port identifier of the <a href="https://www.rfc-editor.org/rfc/rfc9110.html#name-uri-origin">"URI origin"</a>
        * HTTP request is sent to.
        *
        * @note
        *   <p> When observed from the client side, and when communicating through an intermediary, `server.port` SHOULD
        *   represent the server port behind any intermediaries, for example proxies, if it's available.
        */
      val serverPort: AttributeSpec[Long] =
        AttributeSpec(
          ServerAttributes.ServerPort,
          List(
            80,
            8080,
            443,
          ),
          Requirement.required,
          Stability.stable
        )

      /** The <a href="https://www.rfc-editor.org/rfc/rfc3986#section-3.1">URI scheme</a> component identifying the used
        * protocol.
        */
      val urlScheme: AttributeSpec[String] =
        AttributeSpec(
          UrlAttributes.UrlScheme,
          List(
            "http",
            "https",
          ),
          Requirement.optIn,
          Stability.stable
        )

      /** The low-cardinality template of an <a href="https://www.rfc-editor.org/rfc/rfc3986#section-4.2">absolute path
        * reference</a>.
        *
        * @note
        *   <p> The `url.template` MUST have low cardinality. It is not usually available on HTTP clients, but may be
        *   known by the application or specialized HTTP instrumentation.
        */
      val urlTemplate: AttributeSpec[String] =
        AttributeSpec(
          UrlExperimentalAttributes.UrlTemplate,
          List(
            "/users/{id}",
            "/users/:id",
            "/users?id={id}",
          ),
          Requirement.conditionallyRequired("If available."),
          Stability.development
        )

      val specs: List[AttributeSpec[_]] =
        List(
          httpRequestMethod,
          serverAddress,
          serverPort,
          urlScheme,
          urlTemplate,
        )
    }

    def create[F[_]: Meter, A: MeasurementValue]: F[UpDownCounter[F, A]] =
      Meter[F]
        .upDownCounter[A](name)
        .withDescription(description)
        .withUnit(unit)
        .create

    def createObserver[F[_]: Meter, A: MeasurementValue]: F[ObservableMeasurement[F, A]] =
      Meter[F]
        .observableUpDownCounter[A](name)
        .withDescription(description)
        .withUnit(unit)
        .createObserver

    def createWithCallback[F[_]: Meter, A: MeasurementValue](
        callback: ObservableMeasurement[F, A] => F[Unit]
    ): Resource[F, ObservableUpDownCounter] =
      Meter[F]
        .observableUpDownCounter[A](name)
        .withDescription(description)
        .withUnit(unit)
        .createWithCallback(callback)

  }

  /** The duration of the successfully established outbound HTTP connections.
    */
  object ClientConnectionDuration extends MetricSpec.Unsealed {

    val name: String = "http.client.connection.duration"
    val description: String = "The duration of the successfully established outbound HTTP connections."
    val unit: String = "s"
    val stability: Stability = Stability.development
    val attributeSpecs: List[AttributeSpec[_]] = AttributeSpecs.specs

    object AttributeSpecs {

      /** Peer address of the network connection - IP address or Unix domain socket name.
        */
      val networkPeerAddress: AttributeSpec[String] =
        AttributeSpec(
          NetworkAttributes.NetworkPeerAddress,
          List(
            "10.1.2.80",
            "/tmp/my.sock",
          ),
          Requirement.recommended,
          Stability.stable
        )

      /** The actual version of the protocol used for network communication.
        *
        * @note
        *   <p> If protocol version is subject to negotiation (for example using <a
        *   href="https://www.rfc-editor.org/rfc/rfc7301.html">ALPN</a>), this attribute SHOULD be set to the negotiated
        *   version. If the actual protocol version is not known, this attribute SHOULD NOT be set.
        */
      val networkProtocolVersion: AttributeSpec[String] =
        AttributeSpec(
          NetworkAttributes.NetworkProtocolVersion,
          List(
            "1.1",
            "2",
          ),
          Requirement.recommended,
          Stability.stable
        )

      /** Server domain name if available without reverse DNS lookup; otherwise, IP address or Unix domain socket name.
        *
        * @note
        *   <p> When observed from the client side, and when communicating through an intermediary, `server.address`
        *   SHOULD represent the server address behind any intermediaries, for example proxies, if it's available.
        */
      val serverAddress: AttributeSpec[String] =
        AttributeSpec(
          ServerAttributes.ServerAddress,
          List(
            "example.com",
            "10.1.2.80",
            "/tmp/my.sock",
          ),
          Requirement.required,
          Stability.stable
        )

      /** Port identifier of the <a href="https://www.rfc-editor.org/rfc/rfc9110.html#name-uri-origin">"URI origin"</a>
        * HTTP request is sent to.
        *
        * @note
        *   <p> When observed from the client side, and when communicating through an intermediary, `server.port` SHOULD
        *   represent the server port behind any intermediaries, for example proxies, if it's available.
        */
      val serverPort: AttributeSpec[Long] =
        AttributeSpec(
          ServerAttributes.ServerPort,
          List(
            80,
            8080,
            443,
          ),
          Requirement.required,
          Stability.stable
        )

      /** The <a href="https://www.rfc-editor.org/rfc/rfc3986#section-3.1">URI scheme</a> component identifying the used
        * protocol.
        */
      val urlScheme: AttributeSpec[String] =
        AttributeSpec(
          UrlAttributes.UrlScheme,
          List(
            "http",
            "https",
          ),
          Requirement.optIn,
          Stability.stable
        )

      val specs: List[AttributeSpec[_]] =
        List(
          networkPeerAddress,
          networkProtocolVersion,
          serverAddress,
          serverPort,
          urlScheme,
        )
    }

    def create[F[_]: Meter, A: MeasurementValue](boundaries: BucketBoundaries): F[Histogram[F, A]] =
      Meter[F]
        .histogram[A](name)
        .withDescription(description)
        .withUnit(unit)
        .withExplicitBucketBoundaries(boundaries)
        .create

  }

  /** Number of outbound HTTP connections that are currently active or idle on the client.
    */
  object ClientOpenConnections extends MetricSpec.Unsealed {

    val name: String = "http.client.open_connections"
    val description: String = "Number of outbound HTTP connections that are currently active or idle on the client."
    val unit: String = "{connection}"
    val stability: Stability = Stability.development
    val attributeSpecs: List[AttributeSpec[_]] = AttributeSpecs.specs

    object AttributeSpecs {

      /** State of the HTTP connection in the HTTP connection pool.
        */
      val httpConnectionState: AttributeSpec[String] =
        AttributeSpec(
          HttpExperimentalAttributes.HttpConnectionState,
          List(
            "active",
            "idle",
          ),
          Requirement.required,
          Stability.development
        )

      /** Peer address of the network connection - IP address or Unix domain socket name.
        */
      val networkPeerAddress: AttributeSpec[String] =
        AttributeSpec(
          NetworkAttributes.NetworkPeerAddress,
          List(
            "10.1.2.80",
            "/tmp/my.sock",
          ),
          Requirement.recommended,
          Stability.stable
        )

      /** The actual version of the protocol used for network communication.
        *
        * @note
        *   <p> If protocol version is subject to negotiation (for example using <a
        *   href="https://www.rfc-editor.org/rfc/rfc7301.html">ALPN</a>), this attribute SHOULD be set to the negotiated
        *   version. If the actual protocol version is not known, this attribute SHOULD NOT be set.
        */
      val networkProtocolVersion: AttributeSpec[String] =
        AttributeSpec(
          NetworkAttributes.NetworkProtocolVersion,
          List(
            "1.1",
            "2",
          ),
          Requirement.recommended,
          Stability.stable
        )

      /** Server domain name if available without reverse DNS lookup; otherwise, IP address or Unix domain socket name.
        *
        * @note
        *   <p> When observed from the client side, and when communicating through an intermediary, `server.address`
        *   SHOULD represent the server address behind any intermediaries, for example proxies, if it's available.
        */
      val serverAddress: AttributeSpec[String] =
        AttributeSpec(
          ServerAttributes.ServerAddress,
          List(
            "example.com",
            "10.1.2.80",
            "/tmp/my.sock",
          ),
          Requirement.required,
          Stability.stable
        )

      /** Port identifier of the <a href="https://www.rfc-editor.org/rfc/rfc9110.html#name-uri-origin">"URI origin"</a>
        * HTTP request is sent to.
        *
        * @note
        *   <p> When observed from the client side, and when communicating through an intermediary, `server.port` SHOULD
        *   represent the server port behind any intermediaries, for example proxies, if it's available.
        */
      val serverPort: AttributeSpec[Long] =
        AttributeSpec(
          ServerAttributes.ServerPort,
          List(
            80,
            8080,
            443,
          ),
          Requirement.required,
          Stability.stable
        )

      /** The <a href="https://www.rfc-editor.org/rfc/rfc3986#section-3.1">URI scheme</a> component identifying the used
        * protocol.
        */
      val urlScheme: AttributeSpec[String] =
        AttributeSpec(
          UrlAttributes.UrlScheme,
          List(
            "http",
            "https",
          ),
          Requirement.optIn,
          Stability.stable
        )

      val specs: List[AttributeSpec[_]] =
        List(
          httpConnectionState,
          networkPeerAddress,
          networkProtocolVersion,
          serverAddress,
          serverPort,
          urlScheme,
        )
    }

    def create[F[_]: Meter, A: MeasurementValue]: F[UpDownCounter[F, A]] =
      Meter[F]
        .upDownCounter[A](name)
        .withDescription(description)
        .withUnit(unit)
        .create

    def createObserver[F[_]: Meter, A: MeasurementValue]: F[ObservableMeasurement[F, A]] =
      Meter[F]
        .observableUpDownCounter[A](name)
        .withDescription(description)
        .withUnit(unit)
        .createObserver

    def createWithCallback[F[_]: Meter, A: MeasurementValue](
        callback: ObservableMeasurement[F, A] => F[Unit]
    ): Resource[F, ObservableUpDownCounter] =
      Meter[F]
        .observableUpDownCounter[A](name)
        .withDescription(description)
        .withUnit(unit)
        .createWithCallback(callback)

  }

  /** Size of HTTP client request bodies.
    *
    * @note
    *   <p> The size of the request payload body in bytes. This is the number of bytes transferred excluding headers and
    *   is often, but not always, present as the <a
    *   href="https://www.rfc-editor.org/rfc/rfc9110.html#field.content-length">Content-Length</a> header. For requests
    *   using transport encoding, this should be the compressed size.
    */
  object ClientRequestBodySize extends MetricSpec.Unsealed {

    val name: String = "http.client.request.body.size"
    val description: String = "Size of HTTP client request bodies."
    val unit: String = "By"
    val stability: Stability = Stability.development
    val attributeSpecs: List[AttributeSpec[_]] = AttributeSpecs.specs

    object AttributeSpecs {

      /** Describes a class of error the operation ended with.
        *
        * @note
        *   <p> If the request fails with an error before response status code was sent or received, `error.type` SHOULD
        *   be set to exception type (its fully-qualified class name, if applicable) or a component-specific low
        *   cardinality error identifier. <p> If response status code was sent or received and status indicates an error
        *   according to <a href="/docs/http/http-spans.md">HTTP span status definition</a>, `error.type` SHOULD be set
        *   to the status code number (represented as a string), an exception type (if thrown) or a component-specific
        *   error identifier. <p> The `error.type` value SHOULD be predictable and SHOULD have low cardinality.
        *   Instrumentations SHOULD document the list of errors they report. <p> The cardinality of `error.type` within
        *   one instrumentation library SHOULD be low, but telemetry consumers that aggregate data from multiple
        *   instrumentation libraries and applications should be prepared for `error.type` to have high cardinality at
        *   query time, when no additional filters are applied. <p> If the request has completed successfully,
        *   instrumentations SHOULD NOT set `error.type`.
        */
      val errorType: AttributeSpec[String] =
        AttributeSpec(
          ErrorAttributes.ErrorType,
          List(
            "timeout",
            "java.net.UnknownHostException",
            "server_certificate_invalid",
            "500",
          ),
          Requirement.conditionallyRequired("If request has ended with an error."),
          Stability.stable
        )

      /** HTTP request method.
        *
        * @note
        *   <p> HTTP request method value SHOULD be "known" to the instrumentation. By default, this convention defines
        *   "known" methods as the ones listed in <a
        *   href="https://www.rfc-editor.org/rfc/rfc9110.html#name-methods">RFC9110</a> and the PATCH method defined in
        *   <a href="https://www.rfc-editor.org/rfc/rfc5789.html">RFC5789</a>. <p> If the HTTP request method is not
        *   known to instrumentation, it MUST set the `http.request.method` attribute to `_OTHER`. <p> If the HTTP
        *   instrumentation could end up converting valid HTTP request methods to `_OTHER`, then it MUST provide a way
        *   to override the list of known HTTP methods. If this override is done via environment variable, then the
        *   environment variable MUST be named OTEL_INSTRUMENTATION_HTTP_KNOWN_METHODS and support a comma-separated
        *   list of case-sensitive known HTTP methods (this list MUST be a full override of the default known method, it
        *   is not a list of known methods in addition to the defaults). <p> HTTP method names are case-sensitive and
        *   `http.request.method` attribute value MUST match a known HTTP method name exactly. Instrumentations for
        *   specific web frameworks that consider HTTP methods to be case insensitive, SHOULD populate a canonical
        *   equivalent. Tracing instrumentations that do so, MUST also set `http.request.method_original` to the
        *   original value.
        */
      val httpRequestMethod: AttributeSpec[String] =
        AttributeSpec(
          HttpAttributes.HttpRequestMethod,
          List(
            "GET",
            "POST",
            "HEAD",
          ),
          Requirement.required,
          Stability.stable
        )

      /** <a href="https://tools.ietf.org/html/rfc7231#section-6">HTTP response status code</a>.
        */
      val httpResponseStatusCode: AttributeSpec[Long] =
        AttributeSpec(
          HttpAttributes.HttpResponseStatusCode,
          List(
            200,
          ),
          Requirement.conditionallyRequired("If and only if one was received/sent."),
          Stability.stable
        )

      /** <a href="https://wikipedia.org/wiki/Application_layer">OSI application layer</a> or non-OSI equivalent.
        *
        * @note
        *   <p> The value SHOULD be normalized to lowercase.
        */
      val networkProtocolName: AttributeSpec[String] =
        AttributeSpec(
          NetworkAttributes.NetworkProtocolName,
          List(
            "http",
            "spdy",
          ),
          Requirement.conditionallyRequired("If not `http` and `network.protocol.version` is set."),
          Stability.stable
        )

      /** The actual version of the protocol used for network communication.
        *
        * @note
        *   <p> If protocol version is subject to negotiation (for example using <a
        *   href="https://www.rfc-editor.org/rfc/rfc7301.html">ALPN</a>), this attribute SHOULD be set to the negotiated
        *   version. If the actual protocol version is not known, this attribute SHOULD NOT be set.
        */
      val networkProtocolVersion: AttributeSpec[String] =
        AttributeSpec(
          NetworkAttributes.NetworkProtocolVersion,
          List(
            "1.0",
            "1.1",
            "2",
            "3",
          ),
          Requirement.recommended,
          Stability.stable
        )

      /** Host identifier of the <a href="https://www.rfc-editor.org/rfc/rfc9110.html#name-uri-origin">"URI origin"</a>
        * HTTP request is sent to.
        *
        * @note
        *   <p> If an HTTP client request is explicitly made to an IP address, e.g. `http://x.x.x.x:8080`, then
        *   `server.address` SHOULD be the IP address `x.x.x.x`. A DNS lookup SHOULD NOT be used.
        */
      val serverAddress: AttributeSpec[String] =
        AttributeSpec(
          ServerAttributes.ServerAddress,
          List(
            "example.com",
            "10.1.2.80",
            "/tmp/my.sock",
          ),
          Requirement.required,
          Stability.stable
        )

      /** Port identifier of the <a href="https://www.rfc-editor.org/rfc/rfc9110.html#name-uri-origin">"URI origin"</a>
        * HTTP request is sent to.
        *
        * @note
        *   <p> When observed from the client side, and when communicating through an intermediary, `server.port` SHOULD
        *   represent the server port behind any intermediaries, for example proxies, if it's available.
        */
      val serverPort: AttributeSpec[Long] =
        AttributeSpec(
          ServerAttributes.ServerPort,
          List(
            80,
            8080,
            443,
          ),
          Requirement.required,
          Stability.stable
        )

      /** The <a href="https://www.rfc-editor.org/rfc/rfc3986#section-3.1">URI scheme</a> component identifying the used
        * protocol.
        */
      val urlScheme: AttributeSpec[String] =
        AttributeSpec(
          UrlAttributes.UrlScheme,
          List(
            "http",
            "https",
          ),
          Requirement.optIn,
          Stability.stable
        )

      /** The low-cardinality template of an <a href="https://www.rfc-editor.org/rfc/rfc3986#section-4.2">absolute path
        * reference</a>.
        *
        * @note
        *   <p> The `url.template` MUST have low cardinality. It is not usually available on HTTP clients, but may be
        *   known by the application or specialized HTTP instrumentation.
        */
      val urlTemplate: AttributeSpec[String] =
        AttributeSpec(
          UrlExperimentalAttributes.UrlTemplate,
          List(
            "/users/{id}",
            "/users/:id",
            "/users?id={id}",
          ),
          Requirement.conditionallyRequired("If available."),
          Stability.development
        )

      val specs: List[AttributeSpec[_]] =
        List(
          errorType,
          httpRequestMethod,
          httpResponseStatusCode,
          networkProtocolName,
          networkProtocolVersion,
          serverAddress,
          serverPort,
          urlScheme,
          urlTemplate,
        )
    }

    def create[F[_]: Meter, A: MeasurementValue](boundaries: BucketBoundaries): F[Histogram[F, A]] =
      Meter[F]
        .histogram[A](name)
        .withDescription(description)
        .withUnit(unit)
        .withExplicitBucketBoundaries(boundaries)
        .create

  }

  /** Duration of HTTP client requests.
    */
  @deprecated("Use stable `org.typelevel.otel4s.semconv.metrics.HttpMetrics.ClientRequestDuration` instead.", "")
  object ClientRequestDuration extends MetricSpec.Unsealed {

    val name: String = "http.client.request.duration"
    val description: String = "Duration of HTTP client requests."
    val unit: String = "s"
    val stability: Stability = Stability.stable
    val attributeSpecs: List[AttributeSpec[_]] = AttributeSpecs.specs

    object AttributeSpecs {

      /** Describes a class of error the operation ended with.
        *
        * @note
        *   <p> If the request fails with an error before response status code was sent or received, `error.type` SHOULD
        *   be set to exception type (its fully-qualified class name, if applicable) or a component-specific low
        *   cardinality error identifier. <p> If response status code was sent or received and status indicates an error
        *   according to <a href="/docs/http/http-spans.md">HTTP span status definition</a>, `error.type` SHOULD be set
        *   to the status code number (represented as a string), an exception type (if thrown) or a component-specific
        *   error identifier. <p> The `error.type` value SHOULD be predictable and SHOULD have low cardinality.
        *   Instrumentations SHOULD document the list of errors they report. <p> The cardinality of `error.type` within
        *   one instrumentation library SHOULD be low, but telemetry consumers that aggregate data from multiple
        *   instrumentation libraries and applications should be prepared for `error.type` to have high cardinality at
        *   query time, when no additional filters are applied. <p> If the request has completed successfully,
        *   instrumentations SHOULD NOT set `error.type`.
        */
      val errorType: AttributeSpec[String] =
        AttributeSpec(
          ErrorAttributes.ErrorType,
          List(
            "timeout",
            "java.net.UnknownHostException",
            "server_certificate_invalid",
            "500",
          ),
          Requirement.conditionallyRequired("If request has ended with an error."),
          Stability.stable
        )

      /** HTTP request method.
        *
        * @note
        *   <p> HTTP request method value SHOULD be "known" to the instrumentation. By default, this convention defines
        *   "known" methods as the ones listed in <a
        *   href="https://www.rfc-editor.org/rfc/rfc9110.html#name-methods">RFC9110</a> and the PATCH method defined in
        *   <a href="https://www.rfc-editor.org/rfc/rfc5789.html">RFC5789</a>. <p> If the HTTP request method is not
        *   known to instrumentation, it MUST set the `http.request.method` attribute to `_OTHER`. <p> If the HTTP
        *   instrumentation could end up converting valid HTTP request methods to `_OTHER`, then it MUST provide a way
        *   to override the list of known HTTP methods. If this override is done via environment variable, then the
        *   environment variable MUST be named OTEL_INSTRUMENTATION_HTTP_KNOWN_METHODS and support a comma-separated
        *   list of case-sensitive known HTTP methods (this list MUST be a full override of the default known method, it
        *   is not a list of known methods in addition to the defaults). <p> HTTP method names are case-sensitive and
        *   `http.request.method` attribute value MUST match a known HTTP method name exactly. Instrumentations for
        *   specific web frameworks that consider HTTP methods to be case insensitive, SHOULD populate a canonical
        *   equivalent. Tracing instrumentations that do so, MUST also set `http.request.method_original` to the
        *   original value.
        */
      val httpRequestMethod: AttributeSpec[String] =
        AttributeSpec(
          HttpAttributes.HttpRequestMethod,
          List(
            "GET",
            "POST",
            "HEAD",
          ),
          Requirement.required,
          Stability.stable
        )

      /** <a href="https://tools.ietf.org/html/rfc7231#section-6">HTTP response status code</a>.
        */
      val httpResponseStatusCode: AttributeSpec[Long] =
        AttributeSpec(
          HttpAttributes.HttpResponseStatusCode,
          List(
            200,
          ),
          Requirement.conditionallyRequired("If and only if one was received/sent."),
          Stability.stable
        )

      /** <a href="https://wikipedia.org/wiki/Application_layer">OSI application layer</a> or non-OSI equivalent.
        *
        * @note
        *   <p> The value SHOULD be normalized to lowercase.
        */
      val networkProtocolName: AttributeSpec[String] =
        AttributeSpec(
          NetworkAttributes.NetworkProtocolName,
          List(
            "http",
            "spdy",
          ),
          Requirement.conditionallyRequired("If not `http` and `network.protocol.version` is set."),
          Stability.stable
        )

      /** The actual version of the protocol used for network communication.
        *
        * @note
        *   <p> If protocol version is subject to negotiation (for example using <a
        *   href="https://www.rfc-editor.org/rfc/rfc7301.html">ALPN</a>), this attribute SHOULD be set to the negotiated
        *   version. If the actual protocol version is not known, this attribute SHOULD NOT be set.
        */
      val networkProtocolVersion: AttributeSpec[String] =
        AttributeSpec(
          NetworkAttributes.NetworkProtocolVersion,
          List(
            "1.0",
            "1.1",
            "2",
            "3",
          ),
          Requirement.recommended,
          Stability.stable
        )

      /** Host identifier of the <a href="https://www.rfc-editor.org/rfc/rfc9110.html#name-uri-origin">"URI origin"</a>
        * HTTP request is sent to.
        *
        * @note
        *   <p> If an HTTP client request is explicitly made to an IP address, e.g. `http://x.x.x.x:8080`, then
        *   `server.address` SHOULD be the IP address `x.x.x.x`. A DNS lookup SHOULD NOT be used.
        */
      val serverAddress: AttributeSpec[String] =
        AttributeSpec(
          ServerAttributes.ServerAddress,
          List(
            "example.com",
            "10.1.2.80",
            "/tmp/my.sock",
          ),
          Requirement.required,
          Stability.stable
        )

      /** Port identifier of the <a href="https://www.rfc-editor.org/rfc/rfc9110.html#name-uri-origin">"URI origin"</a>
        * HTTP request is sent to.
        *
        * @note
        *   <p> When observed from the client side, and when communicating through an intermediary, `server.port` SHOULD
        *   represent the server port behind any intermediaries, for example proxies, if it's available.
        */
      val serverPort: AttributeSpec[Long] =
        AttributeSpec(
          ServerAttributes.ServerPort,
          List(
            80,
            8080,
            443,
          ),
          Requirement.required,
          Stability.stable
        )

      /** The <a href="https://www.rfc-editor.org/rfc/rfc3986#section-3.1">URI scheme</a> component identifying the used
        * protocol.
        */
      val urlScheme: AttributeSpec[String] =
        AttributeSpec(
          UrlAttributes.UrlScheme,
          List(
            "http",
            "https",
          ),
          Requirement.optIn,
          Stability.stable
        )

      /** The low-cardinality template of an <a href="https://www.rfc-editor.org/rfc/rfc3986#section-4.2">absolute path
        * reference</a>.
        *
        * @note
        *   <p> The `url.template` MUST have low cardinality. It is not usually available on HTTP clients, but may be
        *   known by the application or specialized HTTP instrumentation.
        */
      val urlTemplate: AttributeSpec[String] =
        AttributeSpec(
          UrlExperimentalAttributes.UrlTemplate,
          List(
            "/users/{id}",
            "/users/:id",
            "/users?id={id}",
          ),
          Requirement.optIn,
          Stability.development
        )

      val specs: List[AttributeSpec[_]] =
        List(
          errorType,
          httpRequestMethod,
          httpResponseStatusCode,
          networkProtocolName,
          networkProtocolVersion,
          serverAddress,
          serverPort,
          urlScheme,
          urlTemplate,
        )
    }

    def create[F[_]: Meter, A: MeasurementValue](boundaries: BucketBoundaries): F[Histogram[F, A]] =
      Meter[F]
        .histogram[A](name)
        .withDescription(description)
        .withUnit(unit)
        .withExplicitBucketBoundaries(boundaries)
        .create

  }

  /** Size of HTTP client response bodies.
    *
    * @note
    *   <p> The size of the response payload body in bytes. This is the number of bytes transferred excluding headers
    *   and is often, but not always, present as the <a
    *   href="https://www.rfc-editor.org/rfc/rfc9110.html#field.content-length">Content-Length</a> header. For requests
    *   using transport encoding, this should be the compressed size.
    */
  object ClientResponseBodySize extends MetricSpec.Unsealed {

    val name: String = "http.client.response.body.size"
    val description: String = "Size of HTTP client response bodies."
    val unit: String = "By"
    val stability: Stability = Stability.development
    val attributeSpecs: List[AttributeSpec[_]] = AttributeSpecs.specs

    object AttributeSpecs {

      /** Describes a class of error the operation ended with.
        *
        * @note
        *   <p> If the request fails with an error before response status code was sent or received, `error.type` SHOULD
        *   be set to exception type (its fully-qualified class name, if applicable) or a component-specific low
        *   cardinality error identifier. <p> If response status code was sent or received and status indicates an error
        *   according to <a href="/docs/http/http-spans.md">HTTP span status definition</a>, `error.type` SHOULD be set
        *   to the status code number (represented as a string), an exception type (if thrown) or a component-specific
        *   error identifier. <p> The `error.type` value SHOULD be predictable and SHOULD have low cardinality.
        *   Instrumentations SHOULD document the list of errors they report. <p> The cardinality of `error.type` within
        *   one instrumentation library SHOULD be low, but telemetry consumers that aggregate data from multiple
        *   instrumentation libraries and applications should be prepared for `error.type` to have high cardinality at
        *   query time, when no additional filters are applied. <p> If the request has completed successfully,
        *   instrumentations SHOULD NOT set `error.type`.
        */
      val errorType: AttributeSpec[String] =
        AttributeSpec(
          ErrorAttributes.ErrorType,
          List(
            "timeout",
            "java.net.UnknownHostException",
            "server_certificate_invalid",
            "500",
          ),
          Requirement.conditionallyRequired("If request has ended with an error."),
          Stability.stable
        )

      /** HTTP request method.
        *
        * @note
        *   <p> HTTP request method value SHOULD be "known" to the instrumentation. By default, this convention defines
        *   "known" methods as the ones listed in <a
        *   href="https://www.rfc-editor.org/rfc/rfc9110.html#name-methods">RFC9110</a> and the PATCH method defined in
        *   <a href="https://www.rfc-editor.org/rfc/rfc5789.html">RFC5789</a>. <p> If the HTTP request method is not
        *   known to instrumentation, it MUST set the `http.request.method` attribute to `_OTHER`. <p> If the HTTP
        *   instrumentation could end up converting valid HTTP request methods to `_OTHER`, then it MUST provide a way
        *   to override the list of known HTTP methods. If this override is done via environment variable, then the
        *   environment variable MUST be named OTEL_INSTRUMENTATION_HTTP_KNOWN_METHODS and support a comma-separated
        *   list of case-sensitive known HTTP methods (this list MUST be a full override of the default known method, it
        *   is not a list of known methods in addition to the defaults). <p> HTTP method names are case-sensitive and
        *   `http.request.method` attribute value MUST match a known HTTP method name exactly. Instrumentations for
        *   specific web frameworks that consider HTTP methods to be case insensitive, SHOULD populate a canonical
        *   equivalent. Tracing instrumentations that do so, MUST also set `http.request.method_original` to the
        *   original value.
        */
      val httpRequestMethod: AttributeSpec[String] =
        AttributeSpec(
          HttpAttributes.HttpRequestMethod,
          List(
            "GET",
            "POST",
            "HEAD",
          ),
          Requirement.required,
          Stability.stable
        )

      /** <a href="https://tools.ietf.org/html/rfc7231#section-6">HTTP response status code</a>.
        */
      val httpResponseStatusCode: AttributeSpec[Long] =
        AttributeSpec(
          HttpAttributes.HttpResponseStatusCode,
          List(
            200,
          ),
          Requirement.conditionallyRequired("If and only if one was received/sent."),
          Stability.stable
        )

      /** <a href="https://wikipedia.org/wiki/Application_layer">OSI application layer</a> or non-OSI equivalent.
        *
        * @note
        *   <p> The value SHOULD be normalized to lowercase.
        */
      val networkProtocolName: AttributeSpec[String] =
        AttributeSpec(
          NetworkAttributes.NetworkProtocolName,
          List(
            "http",
            "spdy",
          ),
          Requirement.conditionallyRequired("If not `http` and `network.protocol.version` is set."),
          Stability.stable
        )

      /** The actual version of the protocol used for network communication.
        *
        * @note
        *   <p> If protocol version is subject to negotiation (for example using <a
        *   href="https://www.rfc-editor.org/rfc/rfc7301.html">ALPN</a>), this attribute SHOULD be set to the negotiated
        *   version. If the actual protocol version is not known, this attribute SHOULD NOT be set.
        */
      val networkProtocolVersion: AttributeSpec[String] =
        AttributeSpec(
          NetworkAttributes.NetworkProtocolVersion,
          List(
            "1.0",
            "1.1",
            "2",
            "3",
          ),
          Requirement.recommended,
          Stability.stable
        )

      /** Host identifier of the <a href="https://www.rfc-editor.org/rfc/rfc9110.html#name-uri-origin">"URI origin"</a>
        * HTTP request is sent to.
        *
        * @note
        *   <p> If an HTTP client request is explicitly made to an IP address, e.g. `http://x.x.x.x:8080`, then
        *   `server.address` SHOULD be the IP address `x.x.x.x`. A DNS lookup SHOULD NOT be used.
        */
      val serverAddress: AttributeSpec[String] =
        AttributeSpec(
          ServerAttributes.ServerAddress,
          List(
            "example.com",
            "10.1.2.80",
            "/tmp/my.sock",
          ),
          Requirement.required,
          Stability.stable
        )

      /** Port identifier of the <a href="https://www.rfc-editor.org/rfc/rfc9110.html#name-uri-origin">"URI origin"</a>
        * HTTP request is sent to.
        *
        * @note
        *   <p> When observed from the client side, and when communicating through an intermediary, `server.port` SHOULD
        *   represent the server port behind any intermediaries, for example proxies, if it's available.
        */
      val serverPort: AttributeSpec[Long] =
        AttributeSpec(
          ServerAttributes.ServerPort,
          List(
            80,
            8080,
            443,
          ),
          Requirement.required,
          Stability.stable
        )

      /** The <a href="https://www.rfc-editor.org/rfc/rfc3986#section-3.1">URI scheme</a> component identifying the used
        * protocol.
        */
      val urlScheme: AttributeSpec[String] =
        AttributeSpec(
          UrlAttributes.UrlScheme,
          List(
            "http",
            "https",
          ),
          Requirement.optIn,
          Stability.stable
        )

      /** The low-cardinality template of an <a href="https://www.rfc-editor.org/rfc/rfc3986#section-4.2">absolute path
        * reference</a>.
        *
        * @note
        *   <p> The `url.template` MUST have low cardinality. It is not usually available on HTTP clients, but may be
        *   known by the application or specialized HTTP instrumentation.
        */
      val urlTemplate: AttributeSpec[String] =
        AttributeSpec(
          UrlExperimentalAttributes.UrlTemplate,
          List(
            "/users/{id}",
            "/users/:id",
            "/users?id={id}",
          ),
          Requirement.conditionallyRequired("If available."),
          Stability.development
        )

      val specs: List[AttributeSpec[_]] =
        List(
          errorType,
          httpRequestMethod,
          httpResponseStatusCode,
          networkProtocolName,
          networkProtocolVersion,
          serverAddress,
          serverPort,
          urlScheme,
          urlTemplate,
        )
    }

    def create[F[_]: Meter, A: MeasurementValue](boundaries: BucketBoundaries): F[Histogram[F, A]] =
      Meter[F]
        .histogram[A](name)
        .withDescription(description)
        .withUnit(unit)
        .withExplicitBucketBoundaries(boundaries)
        .create

  }

  /** Number of active HTTP server requests.
    */
  object ServerActiveRequests extends MetricSpec.Unsealed {

    val name: String = "http.server.active_requests"
    val description: String = "Number of active HTTP server requests."
    val unit: String = "{request}"
    val stability: Stability = Stability.development
    val attributeSpecs: List[AttributeSpec[_]] = AttributeSpecs.specs

    object AttributeSpecs {

      /** HTTP request method.
        *
        * @note
        *   <p> HTTP request method value SHOULD be "known" to the instrumentation. By default, this convention defines
        *   "known" methods as the ones listed in <a
        *   href="https://www.rfc-editor.org/rfc/rfc9110.html#name-methods">RFC9110</a> and the PATCH method defined in
        *   <a href="https://www.rfc-editor.org/rfc/rfc5789.html">RFC5789</a>. <p> If the HTTP request method is not
        *   known to instrumentation, it MUST set the `http.request.method` attribute to `_OTHER`. <p> If the HTTP
        *   instrumentation could end up converting valid HTTP request methods to `_OTHER`, then it MUST provide a way
        *   to override the list of known HTTP methods. If this override is done via environment variable, then the
        *   environment variable MUST be named OTEL_INSTRUMENTATION_HTTP_KNOWN_METHODS and support a comma-separated
        *   list of case-sensitive known HTTP methods (this list MUST be a full override of the default known method, it
        *   is not a list of known methods in addition to the defaults). <p> HTTP method names are case-sensitive and
        *   `http.request.method` attribute value MUST match a known HTTP method name exactly. Instrumentations for
        *   specific web frameworks that consider HTTP methods to be case insensitive, SHOULD populate a canonical
        *   equivalent. Tracing instrumentations that do so, MUST also set `http.request.method_original` to the
        *   original value.
        */
      val httpRequestMethod: AttributeSpec[String] =
        AttributeSpec(
          HttpAttributes.HttpRequestMethod,
          List(
            "GET",
            "POST",
            "HEAD",
          ),
          Requirement.required,
          Stability.stable
        )

      /** Name of the local HTTP server that received the request.
        *
        * @note
        *   <p> See <a href="/docs/http/http-spans.md#setting-serveraddress-and-serverport-attributes">Setting
        *   `server.address` and `server.port` attributes</a>. <blockquote> <strong>Warning</strong> Since this
        *   attribute is based on HTTP headers, opting in to it may allow an attacker to trigger cardinality limits,
        *   degrading the usefulness of the metric.</blockquote>
        */
      val serverAddress: AttributeSpec[String] =
        AttributeSpec(
          ServerAttributes.ServerAddress,
          List(
            "example.com",
            "10.1.2.80",
            "/tmp/my.sock",
          ),
          Requirement.optIn,
          Stability.stable
        )

      /** Port of the local HTTP server that received the request.
        *
        * @note
        *   <p> See <a href="/docs/http/http-spans.md#setting-serveraddress-and-serverport-attributes">Setting
        *   `server.address` and `server.port` attributes</a>. <blockquote> <strong>Warning</strong> Since this
        *   attribute is based on HTTP headers, opting in to it may allow an attacker to trigger cardinality limits,
        *   degrading the usefulness of the metric.</blockquote>
        */
      val serverPort: AttributeSpec[Long] =
        AttributeSpec(
          ServerAttributes.ServerPort,
          List(
            80,
            8080,
            443,
          ),
          Requirement.optIn,
          Stability.stable
        )

      /** The <a href="https://www.rfc-editor.org/rfc/rfc3986#section-3.1">URI scheme</a> component identifying the used
        * protocol.
        */
      val urlScheme: AttributeSpec[String] =
        AttributeSpec(
          UrlAttributes.UrlScheme,
          List(
            "http",
            "https",
          ),
          Requirement.required,
          Stability.stable
        )

      val specs: List[AttributeSpec[_]] =
        List(
          httpRequestMethod,
          serverAddress,
          serverPort,
          urlScheme,
        )
    }

    def create[F[_]: Meter, A: MeasurementValue]: F[UpDownCounter[F, A]] =
      Meter[F]
        .upDownCounter[A](name)
        .withDescription(description)
        .withUnit(unit)
        .create

    def createObserver[F[_]: Meter, A: MeasurementValue]: F[ObservableMeasurement[F, A]] =
      Meter[F]
        .observableUpDownCounter[A](name)
        .withDescription(description)
        .withUnit(unit)
        .createObserver

    def createWithCallback[F[_]: Meter, A: MeasurementValue](
        callback: ObservableMeasurement[F, A] => F[Unit]
    ): Resource[F, ObservableUpDownCounter] =
      Meter[F]
        .observableUpDownCounter[A](name)
        .withDescription(description)
        .withUnit(unit)
        .createWithCallback(callback)

  }

  /** Size of HTTP server request bodies.
    *
    * @note
    *   <p> The size of the request payload body in bytes. This is the number of bytes transferred excluding headers and
    *   is often, but not always, present as the <a
    *   href="https://www.rfc-editor.org/rfc/rfc9110.html#field.content-length">Content-Length</a> header. For requests
    *   using transport encoding, this should be the compressed size.
    */
  object ServerRequestBodySize extends MetricSpec.Unsealed {

    val name: String = "http.server.request.body.size"
    val description: String = "Size of HTTP server request bodies."
    val unit: String = "By"
    val stability: Stability = Stability.development
    val attributeSpecs: List[AttributeSpec[_]] = AttributeSpecs.specs

    object AttributeSpecs {

      /** Describes a class of error the operation ended with.
        *
        * @note
        *   <p> If the request fails with an error before response status code was sent or received, `error.type` SHOULD
        *   be set to exception type (its fully-qualified class name, if applicable) or a component-specific low
        *   cardinality error identifier. <p> If response status code was sent or received and status indicates an error
        *   according to <a href="/docs/http/http-spans.md">HTTP span status definition</a>, `error.type` SHOULD be set
        *   to the status code number (represented as a string), an exception type (if thrown) or a component-specific
        *   error identifier. <p> The `error.type` value SHOULD be predictable and SHOULD have low cardinality.
        *   Instrumentations SHOULD document the list of errors they report. <p> The cardinality of `error.type` within
        *   one instrumentation library SHOULD be low, but telemetry consumers that aggregate data from multiple
        *   instrumentation libraries and applications should be prepared for `error.type` to have high cardinality at
        *   query time, when no additional filters are applied. <p> If the request has completed successfully,
        *   instrumentations SHOULD NOT set `error.type`.
        */
      val errorType: AttributeSpec[String] =
        AttributeSpec(
          ErrorAttributes.ErrorType,
          List(
            "timeout",
            "java.net.UnknownHostException",
            "server_certificate_invalid",
            "500",
          ),
          Requirement.conditionallyRequired("If request has ended with an error."),
          Stability.stable
        )

      /** HTTP request method.
        *
        * @note
        *   <p> HTTP request method value SHOULD be "known" to the instrumentation. By default, this convention defines
        *   "known" methods as the ones listed in <a
        *   href="https://www.rfc-editor.org/rfc/rfc9110.html#name-methods">RFC9110</a> and the PATCH method defined in
        *   <a href="https://www.rfc-editor.org/rfc/rfc5789.html">RFC5789</a>. <p> If the HTTP request method is not
        *   known to instrumentation, it MUST set the `http.request.method` attribute to `_OTHER`. <p> If the HTTP
        *   instrumentation could end up converting valid HTTP request methods to `_OTHER`, then it MUST provide a way
        *   to override the list of known HTTP methods. If this override is done via environment variable, then the
        *   environment variable MUST be named OTEL_INSTRUMENTATION_HTTP_KNOWN_METHODS and support a comma-separated
        *   list of case-sensitive known HTTP methods (this list MUST be a full override of the default known method, it
        *   is not a list of known methods in addition to the defaults). <p> HTTP method names are case-sensitive and
        *   `http.request.method` attribute value MUST match a known HTTP method name exactly. Instrumentations for
        *   specific web frameworks that consider HTTP methods to be case insensitive, SHOULD populate a canonical
        *   equivalent. Tracing instrumentations that do so, MUST also set `http.request.method_original` to the
        *   original value.
        */
      val httpRequestMethod: AttributeSpec[String] =
        AttributeSpec(
          HttpAttributes.HttpRequestMethod,
          List(
            "GET",
            "POST",
            "HEAD",
          ),
          Requirement.required,
          Stability.stable
        )

      /** <a href="https://tools.ietf.org/html/rfc7231#section-6">HTTP response status code</a>.
        */
      val httpResponseStatusCode: AttributeSpec[Long] =
        AttributeSpec(
          HttpAttributes.HttpResponseStatusCode,
          List(
            200,
          ),
          Requirement.conditionallyRequired("If and only if one was received/sent."),
          Stability.stable
        )

      /** The matched route, that is, the path template in the format used by the respective server framework.
        *
        * @note
        *   <p> MUST NOT be populated when this is not supported by the HTTP server framework as the route attribute
        *   should have low-cardinality and the URI path can NOT substitute it. SHOULD include the <a
        *   href="/docs/http/http-spans.md#http-server-definitions">application root</a> if there is one.
        */
      val httpRoute: AttributeSpec[String] =
        AttributeSpec(
          HttpAttributes.HttpRoute,
          List(
            "/users/:userID?",
            "{controller}/{action}/{id?}",
          ),
          Requirement.conditionallyRequired("If and only if it's available"),
          Stability.stable
        )

      /** <a href="https://wikipedia.org/wiki/Application_layer">OSI application layer</a> or non-OSI equivalent.
        *
        * @note
        *   <p> The value SHOULD be normalized to lowercase.
        */
      val networkProtocolName: AttributeSpec[String] =
        AttributeSpec(
          NetworkAttributes.NetworkProtocolName,
          List(
            "http",
            "spdy",
          ),
          Requirement.conditionallyRequired("If not `http` and `network.protocol.version` is set."),
          Stability.stable
        )

      /** The actual version of the protocol used for network communication.
        *
        * @note
        *   <p> If protocol version is subject to negotiation (for example using <a
        *   href="https://www.rfc-editor.org/rfc/rfc7301.html">ALPN</a>), this attribute SHOULD be set to the negotiated
        *   version. If the actual protocol version is not known, this attribute SHOULD NOT be set.
        */
      val networkProtocolVersion: AttributeSpec[String] =
        AttributeSpec(
          NetworkAttributes.NetworkProtocolVersion,
          List(
            "1.0",
            "1.1",
            "2",
            "3",
          ),
          Requirement.recommended,
          Stability.stable
        )

      /** Name of the local HTTP server that received the request.
        *
        * @note
        *   <p> See <a href="/docs/http/http-spans.md#setting-serveraddress-and-serverport-attributes">Setting
        *   `server.address` and `server.port` attributes</a>. <blockquote> <strong>Warning</strong> Since this
        *   attribute is based on HTTP headers, opting in to it may allow an attacker to trigger cardinality limits,
        *   degrading the usefulness of the metric.</blockquote>
        */
      val serverAddress: AttributeSpec[String] =
        AttributeSpec(
          ServerAttributes.ServerAddress,
          List(
            "example.com",
            "10.1.2.80",
            "/tmp/my.sock",
          ),
          Requirement.optIn,
          Stability.stable
        )

      /** Port of the local HTTP server that received the request.
        *
        * @note
        *   <p> See <a href="/docs/http/http-spans.md#setting-serveraddress-and-serverport-attributes">Setting
        *   `server.address` and `server.port` attributes</a>. <blockquote> <strong>Warning</strong> Since this
        *   attribute is based on HTTP headers, opting in to it may allow an attacker to trigger cardinality limits,
        *   degrading the usefulness of the metric.</blockquote>
        */
      val serverPort: AttributeSpec[Long] =
        AttributeSpec(
          ServerAttributes.ServerPort,
          List(
            80,
            8080,
            443,
          ),
          Requirement.optIn,
          Stability.stable
        )

      /** The <a href="https://www.rfc-editor.org/rfc/rfc3986#section-3.1">URI scheme</a> component identifying the used
        * protocol.
        *
        * @note
        *   <p> The scheme of the original client request, if known (e.g. from <a
        *   href="https://developer.mozilla.org/docs/Web/HTTP/Headers/Forwarded#proto">Forwarded#proto</a>, <a
        *   href="https://developer.mozilla.org/docs/Web/HTTP/Headers/X-Forwarded-Proto">X-Forwarded-Proto</a>, or a
        *   similar header). Otherwise, the scheme of the immediate peer request.
        */
      val urlScheme: AttributeSpec[String] =
        AttributeSpec(
          UrlAttributes.UrlScheme,
          List(
            "http",
            "https",
          ),
          Requirement.required,
          Stability.stable
        )

      /** Specifies the category of synthetic traffic, such as tests or bots.
        *
        * @note
        *   <p> This attribute MAY be derived from the contents of the `user_agent.original` attribute. Components that
        *   populate the attribute are responsible for determining what they consider to be synthetic bot or test
        *   traffic. This attribute can either be set for self-identification purposes, or on telemetry detected to be
        *   generated as a result of a synthetic request. This attribute is useful for distinguishing between genuine
        *   client traffic and synthetic traffic generated by bots or tests.
        */
      val userAgentSyntheticType: AttributeSpec[String] =
        AttributeSpec(
          UserAgentExperimentalAttributes.UserAgentSyntheticType,
          List(
          ),
          Requirement.optIn,
          Stability.development
        )

      val specs: List[AttributeSpec[_]] =
        List(
          errorType,
          httpRequestMethod,
          httpResponseStatusCode,
          httpRoute,
          networkProtocolName,
          networkProtocolVersion,
          serverAddress,
          serverPort,
          urlScheme,
          userAgentSyntheticType,
        )
    }

    def create[F[_]: Meter, A: MeasurementValue](boundaries: BucketBoundaries): F[Histogram[F, A]] =
      Meter[F]
        .histogram[A](name)
        .withDescription(description)
        .withUnit(unit)
        .withExplicitBucketBoundaries(boundaries)
        .create

  }

  /** Duration of HTTP server requests.
    */
  @deprecated("Use stable `org.typelevel.otel4s.semconv.metrics.HttpMetrics.ServerRequestDuration` instead.", "")
  object ServerRequestDuration extends MetricSpec.Unsealed {

    val name: String = "http.server.request.duration"
    val description: String = "Duration of HTTP server requests."
    val unit: String = "s"
    val stability: Stability = Stability.stable
    val attributeSpecs: List[AttributeSpec[_]] = AttributeSpecs.specs

    object AttributeSpecs {

      /** Describes a class of error the operation ended with.
        *
        * @note
        *   <p> If the request fails with an error before response status code was sent or received, `error.type` SHOULD
        *   be set to exception type (its fully-qualified class name, if applicable) or a component-specific low
        *   cardinality error identifier. <p> If response status code was sent or received and status indicates an error
        *   according to <a href="/docs/http/http-spans.md">HTTP span status definition</a>, `error.type` SHOULD be set
        *   to the status code number (represented as a string), an exception type (if thrown) or a component-specific
        *   error identifier. <p> The `error.type` value SHOULD be predictable and SHOULD have low cardinality.
        *   Instrumentations SHOULD document the list of errors they report. <p> The cardinality of `error.type` within
        *   one instrumentation library SHOULD be low, but telemetry consumers that aggregate data from multiple
        *   instrumentation libraries and applications should be prepared for `error.type` to have high cardinality at
        *   query time, when no additional filters are applied. <p> If the request has completed successfully,
        *   instrumentations SHOULD NOT set `error.type`.
        */
      val errorType: AttributeSpec[String] =
        AttributeSpec(
          ErrorAttributes.ErrorType,
          List(
            "timeout",
            "java.net.UnknownHostException",
            "server_certificate_invalid",
            "500",
          ),
          Requirement.conditionallyRequired("If request has ended with an error."),
          Stability.stable
        )

      /** HTTP request method.
        *
        * @note
        *   <p> HTTP request method value SHOULD be "known" to the instrumentation. By default, this convention defines
        *   "known" methods as the ones listed in <a
        *   href="https://www.rfc-editor.org/rfc/rfc9110.html#name-methods">RFC9110</a> and the PATCH method defined in
        *   <a href="https://www.rfc-editor.org/rfc/rfc5789.html">RFC5789</a>. <p> If the HTTP request method is not
        *   known to instrumentation, it MUST set the `http.request.method` attribute to `_OTHER`. <p> If the HTTP
        *   instrumentation could end up converting valid HTTP request methods to `_OTHER`, then it MUST provide a way
        *   to override the list of known HTTP methods. If this override is done via environment variable, then the
        *   environment variable MUST be named OTEL_INSTRUMENTATION_HTTP_KNOWN_METHODS and support a comma-separated
        *   list of case-sensitive known HTTP methods (this list MUST be a full override of the default known method, it
        *   is not a list of known methods in addition to the defaults). <p> HTTP method names are case-sensitive and
        *   `http.request.method` attribute value MUST match a known HTTP method name exactly. Instrumentations for
        *   specific web frameworks that consider HTTP methods to be case insensitive, SHOULD populate a canonical
        *   equivalent. Tracing instrumentations that do so, MUST also set `http.request.method_original` to the
        *   original value.
        */
      val httpRequestMethod: AttributeSpec[String] =
        AttributeSpec(
          HttpAttributes.HttpRequestMethod,
          List(
            "GET",
            "POST",
            "HEAD",
          ),
          Requirement.required,
          Stability.stable
        )

      /** <a href="https://tools.ietf.org/html/rfc7231#section-6">HTTP response status code</a>.
        */
      val httpResponseStatusCode: AttributeSpec[Long] =
        AttributeSpec(
          HttpAttributes.HttpResponseStatusCode,
          List(
            200,
          ),
          Requirement.conditionallyRequired("If and only if one was received/sent."),
          Stability.stable
        )

      /** The matched route, that is, the path template in the format used by the respective server framework.
        *
        * @note
        *   <p> MUST NOT be populated when this is not supported by the HTTP server framework as the route attribute
        *   should have low-cardinality and the URI path can NOT substitute it. SHOULD include the <a
        *   href="/docs/http/http-spans.md#http-server-definitions">application root</a> if there is one.
        */
      val httpRoute: AttributeSpec[String] =
        AttributeSpec(
          HttpAttributes.HttpRoute,
          List(
            "/users/:userID?",
            "{controller}/{action}/{id?}",
          ),
          Requirement.conditionallyRequired("If and only if it's available"),
          Stability.stable
        )

      /** <a href="https://wikipedia.org/wiki/Application_layer">OSI application layer</a> or non-OSI equivalent.
        *
        * @note
        *   <p> The value SHOULD be normalized to lowercase.
        */
      val networkProtocolName: AttributeSpec[String] =
        AttributeSpec(
          NetworkAttributes.NetworkProtocolName,
          List(
            "http",
            "spdy",
          ),
          Requirement.conditionallyRequired("If not `http` and `network.protocol.version` is set."),
          Stability.stable
        )

      /** The actual version of the protocol used for network communication.
        *
        * @note
        *   <p> If protocol version is subject to negotiation (for example using <a
        *   href="https://www.rfc-editor.org/rfc/rfc7301.html">ALPN</a>), this attribute SHOULD be set to the negotiated
        *   version. If the actual protocol version is not known, this attribute SHOULD NOT be set.
        */
      val networkProtocolVersion: AttributeSpec[String] =
        AttributeSpec(
          NetworkAttributes.NetworkProtocolVersion,
          List(
            "1.0",
            "1.1",
            "2",
            "3",
          ),
          Requirement.recommended,
          Stability.stable
        )

      /** Name of the local HTTP server that received the request.
        *
        * @note
        *   <p> See <a href="/docs/http/http-spans.md#setting-serveraddress-and-serverport-attributes">Setting
        *   `server.address` and `server.port` attributes</a>. <blockquote> <strong>Warning</strong> Since this
        *   attribute is based on HTTP headers, opting in to it may allow an attacker to trigger cardinality limits,
        *   degrading the usefulness of the metric.</blockquote>
        */
      val serverAddress: AttributeSpec[String] =
        AttributeSpec(
          ServerAttributes.ServerAddress,
          List(
            "example.com",
            "10.1.2.80",
            "/tmp/my.sock",
          ),
          Requirement.optIn,
          Stability.stable
        )

      /** Port of the local HTTP server that received the request.
        *
        * @note
        *   <p> See <a href="/docs/http/http-spans.md#setting-serveraddress-and-serverport-attributes">Setting
        *   `server.address` and `server.port` attributes</a>. <blockquote> <strong>Warning</strong> Since this
        *   attribute is based on HTTP headers, opting in to it may allow an attacker to trigger cardinality limits,
        *   degrading the usefulness of the metric.</blockquote>
        */
      val serverPort: AttributeSpec[Long] =
        AttributeSpec(
          ServerAttributes.ServerPort,
          List(
            80,
            8080,
            443,
          ),
          Requirement.optIn,
          Stability.stable
        )

      /** The <a href="https://www.rfc-editor.org/rfc/rfc3986#section-3.1">URI scheme</a> component identifying the used
        * protocol.
        *
        * @note
        *   <p> The scheme of the original client request, if known (e.g. from <a
        *   href="https://developer.mozilla.org/docs/Web/HTTP/Headers/Forwarded#proto">Forwarded#proto</a>, <a
        *   href="https://developer.mozilla.org/docs/Web/HTTP/Headers/X-Forwarded-Proto">X-Forwarded-Proto</a>, or a
        *   similar header). Otherwise, the scheme of the immediate peer request.
        */
      val urlScheme: AttributeSpec[String] =
        AttributeSpec(
          UrlAttributes.UrlScheme,
          List(
            "http",
            "https",
          ),
          Requirement.required,
          Stability.stable
        )

      /** Specifies the category of synthetic traffic, such as tests or bots.
        *
        * @note
        *   <p> This attribute MAY be derived from the contents of the `user_agent.original` attribute. Components that
        *   populate the attribute are responsible for determining what they consider to be synthetic bot or test
        *   traffic. This attribute can either be set for self-identification purposes, or on telemetry detected to be
        *   generated as a result of a synthetic request. This attribute is useful for distinguishing between genuine
        *   client traffic and synthetic traffic generated by bots or tests.
        */
      val userAgentSyntheticType: AttributeSpec[String] =
        AttributeSpec(
          UserAgentExperimentalAttributes.UserAgentSyntheticType,
          List(
          ),
          Requirement.optIn,
          Stability.development
        )

      val specs: List[AttributeSpec[_]] =
        List(
          errorType,
          httpRequestMethod,
          httpResponseStatusCode,
          httpRoute,
          networkProtocolName,
          networkProtocolVersion,
          serverAddress,
          serverPort,
          urlScheme,
          userAgentSyntheticType,
        )
    }

    def create[F[_]: Meter, A: MeasurementValue](boundaries: BucketBoundaries): F[Histogram[F, A]] =
      Meter[F]
        .histogram[A](name)
        .withDescription(description)
        .withUnit(unit)
        .withExplicitBucketBoundaries(boundaries)
        .create

  }

  /** Size of HTTP server response bodies.
    *
    * @note
    *   <p> The size of the response payload body in bytes. This is the number of bytes transferred excluding headers
    *   and is often, but not always, present as the <a
    *   href="https://www.rfc-editor.org/rfc/rfc9110.html#field.content-length">Content-Length</a> header. For requests
    *   using transport encoding, this should be the compressed size.
    */
  object ServerResponseBodySize extends MetricSpec.Unsealed {

    val name: String = "http.server.response.body.size"
    val description: String = "Size of HTTP server response bodies."
    val unit: String = "By"
    val stability: Stability = Stability.development
    val attributeSpecs: List[AttributeSpec[_]] = AttributeSpecs.specs

    object AttributeSpecs {

      /** Describes a class of error the operation ended with.
        *
        * @note
        *   <p> If the request fails with an error before response status code was sent or received, `error.type` SHOULD
        *   be set to exception type (its fully-qualified class name, if applicable) or a component-specific low
        *   cardinality error identifier. <p> If response status code was sent or received and status indicates an error
        *   according to <a href="/docs/http/http-spans.md">HTTP span status definition</a>, `error.type` SHOULD be set
        *   to the status code number (represented as a string), an exception type (if thrown) or a component-specific
        *   error identifier. <p> The `error.type` value SHOULD be predictable and SHOULD have low cardinality.
        *   Instrumentations SHOULD document the list of errors they report. <p> The cardinality of `error.type` within
        *   one instrumentation library SHOULD be low, but telemetry consumers that aggregate data from multiple
        *   instrumentation libraries and applications should be prepared for `error.type` to have high cardinality at
        *   query time, when no additional filters are applied. <p> If the request has completed successfully,
        *   instrumentations SHOULD NOT set `error.type`.
        */
      val errorType: AttributeSpec[String] =
        AttributeSpec(
          ErrorAttributes.ErrorType,
          List(
            "timeout",
            "java.net.UnknownHostException",
            "server_certificate_invalid",
            "500",
          ),
          Requirement.conditionallyRequired("If request has ended with an error."),
          Stability.stable
        )

      /** HTTP request method.
        *
        * @note
        *   <p> HTTP request method value SHOULD be "known" to the instrumentation. By default, this convention defines
        *   "known" methods as the ones listed in <a
        *   href="https://www.rfc-editor.org/rfc/rfc9110.html#name-methods">RFC9110</a> and the PATCH method defined in
        *   <a href="https://www.rfc-editor.org/rfc/rfc5789.html">RFC5789</a>. <p> If the HTTP request method is not
        *   known to instrumentation, it MUST set the `http.request.method` attribute to `_OTHER`. <p> If the HTTP
        *   instrumentation could end up converting valid HTTP request methods to `_OTHER`, then it MUST provide a way
        *   to override the list of known HTTP methods. If this override is done via environment variable, then the
        *   environment variable MUST be named OTEL_INSTRUMENTATION_HTTP_KNOWN_METHODS and support a comma-separated
        *   list of case-sensitive known HTTP methods (this list MUST be a full override of the default known method, it
        *   is not a list of known methods in addition to the defaults). <p> HTTP method names are case-sensitive and
        *   `http.request.method` attribute value MUST match a known HTTP method name exactly. Instrumentations for
        *   specific web frameworks that consider HTTP methods to be case insensitive, SHOULD populate a canonical
        *   equivalent. Tracing instrumentations that do so, MUST also set `http.request.method_original` to the
        *   original value.
        */
      val httpRequestMethod: AttributeSpec[String] =
        AttributeSpec(
          HttpAttributes.HttpRequestMethod,
          List(
            "GET",
            "POST",
            "HEAD",
          ),
          Requirement.required,
          Stability.stable
        )

      /** <a href="https://tools.ietf.org/html/rfc7231#section-6">HTTP response status code</a>.
        */
      val httpResponseStatusCode: AttributeSpec[Long] =
        AttributeSpec(
          HttpAttributes.HttpResponseStatusCode,
          List(
            200,
          ),
          Requirement.conditionallyRequired("If and only if one was received/sent."),
          Stability.stable
        )

      /** The matched route, that is, the path template in the format used by the respective server framework.
        *
        * @note
        *   <p> MUST NOT be populated when this is not supported by the HTTP server framework as the route attribute
        *   should have low-cardinality and the URI path can NOT substitute it. SHOULD include the <a
        *   href="/docs/http/http-spans.md#http-server-definitions">application root</a> if there is one.
        */
      val httpRoute: AttributeSpec[String] =
        AttributeSpec(
          HttpAttributes.HttpRoute,
          List(
            "/users/:userID?",
            "{controller}/{action}/{id?}",
          ),
          Requirement.conditionallyRequired("If and only if it's available"),
          Stability.stable
        )

      /** <a href="https://wikipedia.org/wiki/Application_layer">OSI application layer</a> or non-OSI equivalent.
        *
        * @note
        *   <p> The value SHOULD be normalized to lowercase.
        */
      val networkProtocolName: AttributeSpec[String] =
        AttributeSpec(
          NetworkAttributes.NetworkProtocolName,
          List(
            "http",
            "spdy",
          ),
          Requirement.conditionallyRequired("If not `http` and `network.protocol.version` is set."),
          Stability.stable
        )

      /** The actual version of the protocol used for network communication.
        *
        * @note
        *   <p> If protocol version is subject to negotiation (for example using <a
        *   href="https://www.rfc-editor.org/rfc/rfc7301.html">ALPN</a>), this attribute SHOULD be set to the negotiated
        *   version. If the actual protocol version is not known, this attribute SHOULD NOT be set.
        */
      val networkProtocolVersion: AttributeSpec[String] =
        AttributeSpec(
          NetworkAttributes.NetworkProtocolVersion,
          List(
            "1.0",
            "1.1",
            "2",
            "3",
          ),
          Requirement.recommended,
          Stability.stable
        )

      /** Name of the local HTTP server that received the request.
        *
        * @note
        *   <p> See <a href="/docs/http/http-spans.md#setting-serveraddress-and-serverport-attributes">Setting
        *   `server.address` and `server.port` attributes</a>. <blockquote> <strong>Warning</strong> Since this
        *   attribute is based on HTTP headers, opting in to it may allow an attacker to trigger cardinality limits,
        *   degrading the usefulness of the metric.</blockquote>
        */
      val serverAddress: AttributeSpec[String] =
        AttributeSpec(
          ServerAttributes.ServerAddress,
          List(
            "example.com",
            "10.1.2.80",
            "/tmp/my.sock",
          ),
          Requirement.optIn,
          Stability.stable
        )

      /** Port of the local HTTP server that received the request.
        *
        * @note
        *   <p> See <a href="/docs/http/http-spans.md#setting-serveraddress-and-serverport-attributes">Setting
        *   `server.address` and `server.port` attributes</a>. <blockquote> <strong>Warning</strong> Since this
        *   attribute is based on HTTP headers, opting in to it may allow an attacker to trigger cardinality limits,
        *   degrading the usefulness of the metric.</blockquote>
        */
      val serverPort: AttributeSpec[Long] =
        AttributeSpec(
          ServerAttributes.ServerPort,
          List(
            80,
            8080,
            443,
          ),
          Requirement.optIn,
          Stability.stable
        )

      /** The <a href="https://www.rfc-editor.org/rfc/rfc3986#section-3.1">URI scheme</a> component identifying the used
        * protocol.
        *
        * @note
        *   <p> The scheme of the original client request, if known (e.g. from <a
        *   href="https://developer.mozilla.org/docs/Web/HTTP/Headers/Forwarded#proto">Forwarded#proto</a>, <a
        *   href="https://developer.mozilla.org/docs/Web/HTTP/Headers/X-Forwarded-Proto">X-Forwarded-Proto</a>, or a
        *   similar header). Otherwise, the scheme of the immediate peer request.
        */
      val urlScheme: AttributeSpec[String] =
        AttributeSpec(
          UrlAttributes.UrlScheme,
          List(
            "http",
            "https",
          ),
          Requirement.required,
          Stability.stable
        )

      /** Specifies the category of synthetic traffic, such as tests or bots.
        *
        * @note
        *   <p> This attribute MAY be derived from the contents of the `user_agent.original` attribute. Components that
        *   populate the attribute are responsible for determining what they consider to be synthetic bot or test
        *   traffic. This attribute can either be set for self-identification purposes, or on telemetry detected to be
        *   generated as a result of a synthetic request. This attribute is useful for distinguishing between genuine
        *   client traffic and synthetic traffic generated by bots or tests.
        */
      val userAgentSyntheticType: AttributeSpec[String] =
        AttributeSpec(
          UserAgentExperimentalAttributes.UserAgentSyntheticType,
          List(
          ),
          Requirement.optIn,
          Stability.development
        )

      val specs: List[AttributeSpec[_]] =
        List(
          errorType,
          httpRequestMethod,
          httpResponseStatusCode,
          httpRoute,
          networkProtocolName,
          networkProtocolVersion,
          serverAddress,
          serverPort,
          urlScheme,
          userAgentSyntheticType,
        )
    }

    def create[F[_]: Meter, A: MeasurementValue](boundaries: BucketBoundaries): F[Histogram[F, A]] =
      Meter[F]
        .histogram[A](name)
        .withDescription(description)
        .withUnit(unit)
        .withExplicitBucketBoundaries(boundaries)
        .create

  }

}
