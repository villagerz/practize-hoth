module Adapter.RabbitMQ.Common where

import           ClassyPrelude
import           Network.AMQP
import           Prelude.Unicode ((∘), (∧), (≡))

data State = State
    { spublisherChan :: Channel
    , sconsumerChan  :: Channel
    }

withState ∷ String → Integer → (State → IO a) → IO a
withState url prefetch action = bracket initState destroyState action'
  where
    initState = do
      publisher ← openConnAndChan
      consumer ← openConnAndChan
      return (publisher, consumer)
    openConnAndChan = do
      conn ← openConnection'' ∘ fromURI $ url
      chan ← openChannel conn
      confirmSelect chan False
      qos chan 0 (fromInteger prefetch) True
      return (conn, chan)
    destroyState ((conn1, _), (conn2, _)) = do
      closeConnection conn1
      closeConnection conn2
    action' ((_, pub), (_, cons)) = action (State pub cons)
