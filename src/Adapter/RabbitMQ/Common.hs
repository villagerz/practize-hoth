module Adapter.RabbitMQ.Common where

import           ClassyPrelude
import           Control.Concurrent  (forkIO)
import           Control.Monad.Catch (MonadCatch)
import           Data.Aeson
import           Data.Has
import           Katip
import           Network.AMQP
import           Prelude.Unicode     ((∘), (∧), (≡))
import           UnliftIO.Exception  (tryAny)

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


initTopicExchange ∷ State → Text → IO ()
initTopicExchange s name = let exchange = newExchange {exchangeName = name
                                                 ,exchangeType = "topic"}
                           in declareExchange (spublisherChan s)  exchange

initQueue ∷ State → Text → Text → Text → IO ()
initQueue s qname exname routingkey =
  let queueCfg = newQueue {queueName = qname, queueDurable = False, queueExclusive = True}
      pubchan = sconsumerChan s
  in
    do
      initTopicExchange s exname
      declareQueue pubchan queueCfg
      bindQueue pubchan qname exname routingkey

-- consumeMsgs :: Channel -> Text -> Ack -> ((Message, Envelope) -> IO ()) -> IO ConsumerTag
-- consumeMsgs chan queue ack callback
initConsumer ∷ Channel → Text → (Message → IO Bool) → IO ()
initConsumer c q handle = do
  void ∘ consumeMsgs c q Ack $ \(msg, env) → void ∘ forkIO $ do
    result ← handle msg
    if result then ackEnv env else rejectEnv env False

type Rabbit r m = (Has State r, MonadReader r m, MonadIO m)

publish ∷ (ToJSON a, Rabbit r m) ⇒ Text → Text → a → m ()
publish exchange routingkey payload = do
  (State chan _) ← asks getter
  let msg = newMsg {msgBody = encode payload}
  liftIO ∘ void $ publishMsg chan exchange routingkey msg


-- eithDecode' parses and decodes immediately (eitherDecode just parses, decodes later)
consume ∷ (KatipContext m, FromJSON a, MonadUnliftIO m, MonadCatch m) ⇒ Message → (a → m Bool) → m Bool
consume msg handle =
  case eitherDecode' (msgBody msg) of
    Left err → withMsgAndErr msg err $ do
      $(logTM) ErrorS "Malformed payload. Rejecting"
      return False
    Right payload → do
      result ← tryAny (handle payload)
      case result of
        Left err → withMsgAndErr msg (displayException err) $ do
          $(logTM) ErrorS "There was an exception when processing the msg. Rejecting"
          return False
        Right bool → return bool

withMsgAndErr ∷ (KatipContext m, ToJSON e) ⇒ Message → e → m a → m a
withMsgAndErr msg err =
  katipAddContext (sl "mqMsg" (show msg) <> sl "error" err)

