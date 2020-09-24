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
instead of relying on the exact schema if we start using stable interfaces
that abstract out the underlying schema this could be possible.

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
  'unlock' unprocessed events during shutdown
```

If the system itself changes,

### Stateful components

That state that is currently involved is:
1. Storing the metadata.
1. Storing cli state.
1. Storing console state.
1. Storing Scheduled Trigger and Event Trigger related data.
1. Storing asynchronous actions related data.
1. Storing user events (in the source database).

#### Event trigger related abstractions

1. `insertEvent :: Namespace -> Payload -> Tx EventId`
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
