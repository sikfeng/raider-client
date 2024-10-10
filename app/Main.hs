{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-}

module Main where

import Control.Monad (forever)
import Data.Aeson (Value, ToJSON(..), object, (.=), encode, eitherDecode)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BL
import Network.WebSockets
import Control.Exception (try, finally, SomeException)
import System.Log.Logger
import Options.Applicative
import Control.Concurrent (threadDelay)

data Config = Config
    { host :: String
    , port :: Int
    , path :: T.Text
    }

data Message = Message
    { mainRepoDir :: T.Text
    , method :: T.Text
    , params :: Params
    }

data Params = Params
    { timeout :: Int
    }

instance ToJSON Main.Message where
    toJSON Message{..} = object
        [ "main_repo_dir" .= mainRepoDir
        , "method" .= method
        , "params" .= params
        ]

instance ToJSON Params where
    toJSON Params{..} = object
        [ "timeout" .= timeout
        ]

runWebSocketClient :: Config -> IO ()
runWebSocketClient Config{..} = do
    infoM "WebSocket" $ "Connecting to " ++ host ++ ":" ++ show port ++ T.unpack path
    result <- try $ runClient host port (T.unpack path) wsApp
    case result of
        Left e -> do
            errorM "WebSocket" $ "Connection failed: " ++ show (e :: SomeException)
            infoM "WebSocket" "Retrying in 5 seconds..."
            threadDelay 5000000  -- 5 seconds
            runWebSocketClient Config{..}
        Right _ -> return ()

wsApp :: Connection -> IO ()
wsApp conn = do
    infoM "WebSocket" "Connected!"
    
    let message = Message "/home/sikfeng/raid/auto-sw-dev/tmp_repo/" "init_agent_manager" (Params 10)
    sendJSON conn (toJSON message)
    infoM "WebSocket" "JSON data sent."

    forever (receiveAndLogJSON conn) `finally` do
        infoM "WebSocket" "Closing connection."
        sendClose conn ("Bye!" :: T.Text)

sendJSON :: Connection -> Value -> IO ()
sendJSON conn = sendTextData conn . encode

receiveAndLogJSON :: Connection -> IO ()
receiveAndLogJSON conn = do
    receivedMsg <- receiveData conn
    case eitherDecode (BL.fromStrict receivedMsg) of
        Left err -> errorM "WebSocket" $ "Failed to parse received message as JSON: " ++ err
        Right (json :: Value) -> infoM "WebSocket" $ "Received: " ++ show json

configParser :: Parser Config
configParser = Config
    <$> strOption
        ( long "host"
        <> metavar "HOST"
        <> help "WebSocket server host"
        <> value "localhost" )
    <*> option auto
        ( long "port"
        <> metavar "PORT"
        <> help "WebSocket server port"
        <> value 10000 )
    <*> strOption
        ( long "path"
        <> metavar "PATH"
        <> help "WebSocket server path"
        <> value "/ws/raider-client-sess-id" )

main :: IO ()
main = do
    updateGlobalLogger "WebSocket" (setLevel INFO)
    config <- execParser opts
    runWebSocketClient config
  where
    opts = info (configParser <**> helper)
        ( fullDesc
        <> progDesc "WebSocket client for raider"
        <> header "raider-client - a WebSocket client" )
