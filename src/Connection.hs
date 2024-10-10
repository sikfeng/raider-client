{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Connection
    ( Config(..)
    , Message(..)
    , Params(..)
    , runWebSocketClient
    , createMessage
    , wsApp
    ) where

import Control.Monad (forever)
import Data.Aeson (Value(..), ToJSON(..), object, (.=), encode, eitherDecode)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Network.WebSockets as WS
import Control.Exception (try, finally, SomeException)
import System.Log.Logger
import Control.Concurrent (threadDelay)
import qualified Data.Aeson.KeyMap as KM

data Config = Config
    { host :: String
    , port :: Int
    , path :: T.Text
    , mainRepoDir :: T.Text
    }

data Message = Message
    { messageMainRepoDir :: T.Text
    , method :: T.Text
    , params :: Params
    }

data Params
    = InitParams { timeout :: Int }
    | AskRepoParams { query :: T.Text }
    | InitExternalRepoParams { repoDir :: T.Text }
    | GetExternalRepoParams
    | DisableExternalRepoParams { repoDir :: T.Text }
    | EnableExternalRepoParams { repoDir :: T.Text }
    | GenerateSubtasksParams { objective :: T.Text }
    | RunSubtaskParams { subtask :: T.Text }
    | ShutdownParams

instance ToJSON Message where
    toJSON Message{..} = object
        [ "main_repo_dir" .= messageMainRepoDir
        , "method" .= method
        , "params" .= params
        ]

instance ToJSON Params where
    toJSON (InitParams{..}) = object [ "timeout" .= timeout ]
    toJSON (AskRepoParams{..}) = object [ "query" .= query ]
    toJSON (InitExternalRepoParams{..}) = object [ "repo_dir" .= repoDir ]
    toJSON GetExternalRepoParams = object []
    toJSON (DisableExternalRepoParams{..}) = object [ "repo_dir" .= repoDir ]
    toJSON (EnableExternalRepoParams{..}) = object [ "repo_dir" .= repoDir ]
    toJSON (GenerateSubtasksParams{..}) = object [ "objective" .= objective ]
    toJSON (RunSubtaskParams{..}) = object [ "subtask" .= subtask ]
    toJSON ShutdownParams = object []

runWebSocketClient :: Config -> IO ()
runWebSocketClient Config{..} = do
    infoM "WebSocket" $ "Connecting to " ++ host ++ ":" ++ show port ++ T.unpack path
    result <- try $ WS.runClient host port (T.unpack path) wsApp
    case result of
        Left e -> do
            errorM "WebSocket" $ "Connection failed: " ++ show (e :: SomeException)
        Right _ -> return ()

createMessage :: T.Text -> T.Text -> Params -> Message
createMessage mainRepoDir method params = Message
    { messageMainRepoDir = mainRepoDir
    , method = method
    , params = params
    }

wsApp :: WS.Connection -> IO ()
wsApp conn = do
    infoM "WebSocket" "Connected!"

    let mainRepoDir = "/home/sikfeng/raid/auto-sw-dev/tmp_repo/"

    -- Create a list of messages to send
    let messages =
            [ createMessage mainRepoDir "init_agent_manager" (InitParams 10)
            , createMessage mainRepoDir "ask_repo" (AskRepoParams "What files are in the repository?")
            , createMessage mainRepoDir "init_external_repo_agent" (InitExternalRepoParams "/home/sikfeng/raid/auto-sw-dev/auto-code-rover/")
            , createMessage mainRepoDir "get_external_repo_agents" GetExternalRepoParams
            ]

    -- Send messages sequentially and wait for responses
    mapM_ (sendAndWaitForResponse conn) messages

    infoM "WebSocket" "All messages sent and responses received."
    WS.sendClose conn ("Bye!" :: T.Text)

sendJSON :: WS.Connection -> Value -> IO ()
sendJSON conn = WS.sendTextData conn . encode

sendAndWaitForResponse :: WS.Connection -> Message -> IO ()
sendAndWaitForResponse conn msg = do
    sendJSON conn (toJSON msg)
    infoM "WebSocket" $ "Sent: " ++ T.unpack (method msg)
    waitForEndOfMessage conn

waitForEndOfMessage :: WS.Connection -> IO ()
waitForEndOfMessage conn = do
    receivedMsg <- WS.receiveData conn
    case eitherDecode (BL.fromStrict receivedMsg) of
        Left err -> errorM "WebSocket" $ "Failed to parse received message as JSON: " ++ err
        Right json -> case json of
            Object obj -> case KM.lookup "<END_OF_MESSAGE>" obj of
                Just _ -> return ()  -- End of message received, exit the function
                Nothing -> case KM.lookup "<PING>" obj of
                    Just _ -> waitForEndOfMessage conn  -- Ping received, continue waiting
                    Nothing -> do
                        infoM "WebSocket" $ "Received: " ++ show json
                        waitForEndOfMessage conn  -- Continue waiting for end of message
            _ -> do
                infoM "WebSocket" $ "Received: " ++ show json
                waitForEndOfMessage conn  -- Continue waiting for end of message
