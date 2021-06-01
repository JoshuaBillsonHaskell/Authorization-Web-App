module Adapter.RabbitMQ.Common where

import qualified Data.Word as W
import qualified Data.Text as T
import qualified Network.AMQP as AMQP
import Katip
import Data.Has
import Data.Aeson
import Control.Monad.Reader
import Control.Exception.Safe

data State = State { producerChannel :: AMQP.Channel, consumerChannel :: AMQP.Channel }

type Rabbit r m = (Has State r, MonadReader r m, MonadIO m)

-- Runs An Action Which Requires An Open Producer And Consumer Channel To The RabbitMQ Server
withState :: T.Text -> W.Word16 -> (State -> IO a) -> IO a
withState connName prefetchCount action =
    bracket initConn closeConn action'
    where initConn = do
              producer <- openConnAndChannel
              consumer <- openConnAndChannel
              return (producer, consumer)
          openConnAndChannel = do
              connection <- AMQP.openConnection'' AMQP.defaultConnectionOpts { AMQP.coName = Just connName }
              channel <- AMQP.openChannel connection
              AMQP.confirmSelect channel False
              AMQP.qos channel 0 prefetchCount False
              return (connection, channel)
          closeConn ((producerConn, _), (consumerConn, _)) = do
              AMQP.closeConnection producerConn
              AMQP.closeConnection consumerConn
          action' ((_, producerChannel'), (_, consumerChannel')) = action $ State producerChannel' consumerChannel'


-- Initialize A RabbitMQ Exchange With A Given Name
initExchange :: State -> T.Text -> IO ()
initExchange (State producerChannel' _) name = AMQP.declareExchange producerChannel' opts
    where opts = AMQP.newExchange { AMQP.exchangeName = name, AMQP.exchangeType = "topic" }


-- Initialize A RabbitMQ Queue With A Given Name And Routing Key And Bind It To A Given Exchange
-- Give Queue A Random Name By Passing An Empty String. Returns The Name Of The New Queue.
initQueue :: State -> T.Text -> T.Text -> T.Text -> IO T.Text
initQueue state@(State producerChannel' _) queueName exchangeName routingKey = do
    initExchange state exchangeName
    (qName, _, _) <- AMQP.declareQueue producerChannel' $ AMQP.newQueue { AMQP.queueName = queueName }
    AMQP.bindQueue producerChannel' qName exchangeName routingKey
    return qName


-- Initialize A Message Consumer On The Given Queue
initConsumer :: State -> T.Text -> ((AMQP.Message, AMQP.Envelope) -> IO Bool) -> IO ()
initConsumer (State _ consumerChannel') queueName callback = do
    void $ AMQP.consumeMsgs consumerChannel' queueName AMQP.Ack $ \payload@(_, env) -> do
      callback payload >>= \case
          True -> AMQP.ackEnv env
          False -> AMQP.rejectEnv env False


-- Publish A JSON Object To A Given Exchange With A Given Routing Key
publish :: (ToJSON a, Rabbit r m) => T.Text -> T.Text -> a -> m ()
publish exchange routingKey payload = do
    (State producerChannel' _) <- asks getter
    let msg = AMQP.newMsg { AMQP.msgBody = encode payload }
    liftIO . void $ AMQP.publishMsg producerChannel' exchange routingKey msg


-- Given A RabbitMQ Message Containing A JSON String, Decode The String And Run A Handler Function.
-- Log Any Errors And Return A Boolean Indicating Whether Or Not The Handler Ran Successfully.
consumeAndProcess :: (KatipContext m, MonadCatch m, FromJSON a) => AMQP.Message -> (a -> m Bool) -> m Bool
consumeAndProcess msg handler = do
    -- Attempt To Decode The Message To A FromJSON Instance
    case eitherDecode' (AMQP.msgBody msg) of

        -- Handle The Case Where The Message Could Not Be Decoded
        Left err -> do
            katipAddContext (sl "RabbitMQ Message" (show msg) <> sl "error" err) $ do
                $(logTM) ErrorS "Malformed Message"
                return False

        -- If The Message Was Successfully Decoded, Run The Handler
        Right payload -> do
            tryAny (handler payload) >>= \case

                -- Catch The Case Where The Handler Throws An Error
                Left err -> do
                    katipAddContext (sl "RabbitMQ Message" (show msg) <> sl "error" (show err)) $ do
                        $(logTM) ErrorS "There Was An Error Processing The RabbitMQ Message"
                        return False

                -- If The Handler Ran Successfully, Return The Result
                Right bool -> do
                    return bool