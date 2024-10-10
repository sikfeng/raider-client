{-# LANGUAGE OverloadedStrings #-}

module Main where

import Connection
import Options.Applicative
import System.Log.Logger

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
    <*> strOption
        ( long "main-repo-dir"
        <> metavar "DIR"
        <> help "Main repository directory"
        <> value "/home/sikfeng/raid/auto-sw-dev/tmp_repo/" )

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
