# Trace propagators and cross-service traces

Use [Propagate trace context across service boundaries](../how-to-tracing/propagate-trace-context-across-service-boundaries.md)
for the task-focused steps.

Cross-service tracing works because every service agrees on how to encode and decode trace context in requests,
responses, or messages. That agreement lives in the configured propagators.

## What a propagator controls

A propagator defines:

- which fields are written to an outgoing carrier
- which fields are read from an incoming carrier
- how those fields map back to tracing context

When you call `Tracer[F].propagate`, otel4s asks the configured propagator to write the current trace context into the
carrier you pass in.

When you call `Tracer[F].joinOrRoot`, otel4s asks the configured propagator to read trace context from the carrier and
continue that trace if possible.

That means propagator choice affects both sides of the boundary:

- outgoing calls from your service
- incoming calls into your service

## Default and alternative propagators

`tracecontext` is the standard propagator for parent-child trace context. Unless you configure something else in the
OpenTelemetry SDK, otel4s uses the W3C Trace Context format for trace propagation.

Other common trace-context propagators include:

- `b3`
- `b3multi`
- `jaeger`

`baggage` is related but different. It propagates baggage entries, not parent span context. Many OpenTelemetry setups
enable it alongside a trace-context propagator.

If you need interoperability with another system, configure the matching propagator in the SDK that creates your
`OtelJava` instance.

@:select(config-source)

@:choice(env-vars)

```bash
OTEL_PROPAGATORS=b3multi,tracecontext
```

@:choice(jvm-properties)

```bash
-Dotel.propagators=b3multi,tracecontext
```

@:@

Multiple propagators can be enabled together. That is useful when your service must read and write several formats
during a migration, or when different downstream systems expect different headers.

You can inspect the configured propagators through `Otel4s#propagators`:

```scala mdoc:silent
import cats.effect.IO
import org.typelevel.otel4s.oteljava.OtelJava

OtelJava.autoConfigured[IO]().use { otel4s =>
  IO.println("Propagators: " + otel4s.propagators)
}
```

## Carrier adapters and propagators solve different problems

Two extension points often appear together, but they do different jobs:

- `TextMapGetter` and `TextMapUpdater` adapt your carrier type, such as `Headers` or a message envelope
- `TextMapPropagator` defines the wire format itself

Use `TextMapGetter` and `TextMapUpdater` when your service already uses a standard propagation format, but your carrier
type is not supported out of the box.

Use a custom `TextMapPropagator` when you need a different encoding, a non-standard header scheme, or additional
in-band context fields.

## When a custom propagator makes sense

Most services should stay with the standard propagators. A custom propagator is usually justified only when:

- another system requires a proprietary propagation format
- you need to carry extra context in transport metadata
- you are integrating with an existing protocol that does not use standard trace headers

For example, the propagator below carries a `platform-id` value alongside the normal trace context:

```scala mdoc:reset:silent
import cats.effect.{IO, SyncIO}
import io.opentelemetry.context.propagation.{TextMapPropagator => JTextMapPropagator}
import org.typelevel.otel4s.context.propagation.{TextMapGetter, TextMapPropagator, TextMapUpdater}
import org.typelevel.otel4s.oteljava.OtelJava
import org.typelevel.otel4s.oteljava.context.Context
import org.typelevel.otel4s.oteljava.context.propagation.PropagatorConverters._

object PlatformIdPropagator extends TextMapPropagator[Context] {
  val PlatformIdKey: Context.Key[String] =
    Context.Key.unique[SyncIO, String]("platform-id").unsafeRunSync()

  val fields: Iterable[String] = List("platform-id")

  def extract[A: TextMapGetter](ctx: Context, carrier: A): Context =
    TextMapGetter[A].get(carrier, "platform-id") match {
      case Some(value) => ctx.updated(PlatformIdKey, value)
      case None        => ctx
    }

  def inject[A: TextMapUpdater](ctx: Context, carrier: A): A =
    ctx.get(PlatformIdKey) match {
      case Some(value) => TextMapUpdater[A].updated(carrier, "platform-id", value)
      case None        => carrier
    }
}

OtelJava.autoConfigured[IO] { builder =>
  builder.addPropagatorCustomizer { (current, _) =>
    JTextMapPropagator.composite(current, PlatformIdPropagator.asJava)
  }
}
```

This kind of customization is about interoperability and transport semantics, not about ordinary request handling.
For day-to-day service code, the task-oriented how-to is usually the right page.
