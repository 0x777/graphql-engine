## Zero downtime upgrades of graphql-engine

To have zero downtime when upgrading graphql-engine, both the versions need to
run simultaneously on the same database at some point of time.

When we start a newer version on a database on which an older version is
already running, the newer version `n` upgrades the existing catalog as part of
its startup sequence. The consequences are as follows:

1. There should be no disruption for `v1/graphql` except for async mutations.
Async actions could break if the underlying storage mechanism of the request
changes.

1. The delivery mechanism of events(data, scheduled, cron) and actions on the
older system could fail as they periodically poll to deliver the events. This
however will not deteroirate the event delivery subsystem as the newer version
will start delivering events.

1. Scheduled events related APIs could break when the underlying schema changes.

So currently there is no zero downtime upgrade possible when there are catalog
changes that change the schema related to scheduled events storage and async
actions storage.

How do we get to a state where we can offer zero downtime graphql-engine
upgrades in the presence of catalog upgrades?

### Zero downtime guarantees
What APIs/subystems should not see any downtime when we say zero downtime
upgrades?
1. `v1/graphql` API (including async mutations).
1. Event delivery: Note that event delivery as documented above technically have
   has no downtime but monitoring solutions might pick up those errors.
1. `schedule_event` API

We need not provide any guarantee for APIs which change metadata. I think it is
fair to ask our users not to update their metadata as they are upgrading
versions (it would be great if it can be somehow enforced).

### Approach

We can't really provide zero downtime of async actions and event delivery
unless the newer version can work on the older version's catalog. It is
intractable to support all the previous catalog versions in every version that
we release because our code is written against the current schema. However,
instead of relying on the exact schema if we start using stable interfaces that
abstract out the underlying schema this could be possible.

Let's take our event delivery subsystem - we made a fair number of changes to
the underlying schema, however, the system itself remained pretty much the
same:

```
  forever:
    fetch undelivered events from the queue
    for each event
      try delivering the event
      if the delivery succeeds, mark the delivery as a success
         otherwise mark the delivery as a failure
  unlock unprocessed events during shutdown
```

For the event delivery system, the abstractions are more or less as follows:

1. `insertEvent :: Payload -> Tx EventId`
   Insert an event into the queue with the given namespace
1. `fetchEvents :: Int -> Tx [Event]`:
   Used to fetch events that need to be delivered.
1. `markDeliverySuccess :: EventId -> Tx ()`:
   Called when an event is successfully delivered.
1. `markDeliveryFailure :: EventId -> Tx ()`:
   Called when an event couldn't be delivered with the specified retry configuration.
1. `setDeliveryRetryTime :: EventId -> UTCTime -> Tx ()`:
   Sets the time at which delivery can be attempted again.
1. `reprocessEvent :: EventId -> Tx ()`
   Process the event again for delivery, irrespective of the previous state.

If the entire delivery system is written against this API, these functions can
be generated during the startup based on the catalog version and passed along
to the event delivery subsystem. If say a catalog version cannot provide a
`reprocessEvent` API, we simply initialise it to a function which throws an
error whenever it is invoked (with a helpful message to the user to upgrade the
catalog).

This in itself doesn't completely take away the problem of handling all the
catalog versions, it just makes it more manageable by moving it to the API
initialization phase which would look something like this:

```haskell
fetchEvents :: CatalogVersion -> Int -> Tx [Event]
fetchEvents catalogVersion =
  if | catalogVersion <= 15 -> const $ throwError "unsupported catalog version"
     | catalogVersion > 15 && catalogVersion <= 18 -> ...
     | catalogVersion > 19 && catalogVersion <= 30 -> ...
     | catalogVersion > 30 && catalogVersion < 36 -> ...
     ...
```

We can avoid writing thise functions by moving these abstractions into the database
as Postgres functions, i.e, we can treat the whole event delivery subsystem as a
Postgres extension which provides functions as follows:

1. `uuid insert_event(payload jsonb)`
1. `event[] fetch_undelivered_events(limit integer)`
1. `boolean mark_delivery_success(event_id uuid)`
1. `boolean mark_delivery_failure(event_id uuid)`
...

Instead of having a numeric catalog version, we can have a semantic version to the
event delivery subsystem and the Haskell side of things can be simplified:

```haskell
fetchEvents :: EventDeliveryVersion -> Int -> Tx [Event]
fetchEvents version =
  if | version == 1 -> ...
     | version == 2 -> ...
     ...
```

Given that the general high level abstractions rarely change (that's how it has
been so far) the number of semantic versions to handle will be much lower than
the number of catalog versions.

### Stateful components

We've only looked at event delivery subsystem so far however there are other
components which are also stateful:

#### Scheduled Trigger and Cron Triggers:
These could be treated the same way as the event triggers on tables.

#### Asynchronous actions related data:
Similar approach as event triggers, the APIs that are needed from the
database are:
1. `uuid insert_async_action_request(payload jsonb)`
1. `set_async_action_response(action_id uuid)`
1. `jsonb get_async_action_response(action_id uuid)`

#### Storing metadata, cli and console state:

TODO


