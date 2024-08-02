{-# LANGUAGE OverloadedStrings #-}

module P2P (client, holePuncher) where

import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Data.ByteString.Char8 qualified as BS8
import Network.Socket qualified as NS
import Network.UDP (ClientSockAddr (ClientSockAddr), recvFrom, sendTo, serverSocket)
import System.IO (hFlush, stdout)
import qualified Network.Socket.Address as NS

holePuncherAddress :: String
holePuncherAddress = "127.0.0.1"

holePuncherPort :: NS.PortNumber
holePuncherPort = 4444

client :: String -> String -> IO ()
client holeIp holePort = do
    holeAddr <- head <$> NS.getAddrInfo (Just $ NS.defaultHints {NS.addrSocketType = NS.Datagram}) (Just holeIp) (Just holePort)
    socket <- NS.socket NS.AF_INET NS.Datagram 0
    _ <- NS.sendTo socket "" (NS.addrAddress holeAddr)

    (bs, _ :: NS.SockAddr) <- NS.recvFrom socket 1024
    print bs

    let [ip, port] = map BS8.unpack $ BS8.split ':' bs
    otherAddr <- head <$> NS.getAddrInfo (Just $ NS.defaultHints {NS.addrSocketType = NS.Datagram}) (Just ip) (Just port)

    _ <- forkIO . forever $ do
        (bs, _ :: NS.SockAddr) <- NS.recvFrom socket 1024
        print bs
    forever $ do
        putStr "> "
        hFlush stdout
        bs <- BS8.pack <$> getLine
        _ <- NS.sendTo socket bs (NS.addrAddress otherAddr)
        return ()

holePuncher :: IO ()
holePuncher = do
    socket <- serverSocket (read holePuncherAddress, holePuncherPort)
    loop socket Nothing
  where
    loop socket mClient = do
        (_, client2@(ClientSockAddr client2Addr _)) <- recvFrom socket
        case mClient of
            Nothing -> loop socket $ Just client2
            Just client@(ClientSockAddr clientAddr _) -> do
                sendTo socket (BS8.pack $ show clientAddr) client2
                sendTo socket (BS8.pack $ show client2Addr) client
                loop socket Nothing
