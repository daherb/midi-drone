module Main where

import System.Environment
import Control.Concurrent
import qualified Data.ByteString.Char8 as B

{-
Code is based on alsa-seq examples: list-port.hs, dump.hs
-}

import qualified Sound.ALSA.Sequencer.Client.Info as ClientInfo
import qualified Sound.ALSA.Sequencer.Port.Info as PortInfo
import qualified Sound.ALSA.Sequencer.Connect as Connect
import qualified Sound.ALSA.Sequencer.Address as Addr
import qualified Sound.ALSA.Sequencer.Client as Client
import qualified Sound.ALSA.Sequencer.Event as Event
import qualified Sound.ALSA.Sequencer.Port as Port
import qualified Sound.ALSA.Sequencer as SndSeq
import Text.Printf (printf, )
import Control.Monad.IO.Class (liftIO, )
import Control.Monad (liftM4, join, forever, )

main :: IO ()
main = do
  args <- getArgs
  if length args < 1 then
    do
      putStrLn "Parameters: midi-port channel lowest-note highest-note delay"
      listPorts
  else
    (do
       let channel = if length args >= 2 then
                       read (args !! 1)
                     else
                       0
       let lower = if length args >= 3 then
                      read (args !! 2)
                   else
                      0
       let upper = if length args >= 4 then
                      read (args !! 3)
                   else
                      127
       let delay = if length args >= 5 then
                      read (args !! 4)
                   else
                      300
       SndSeq.withDefault SndSeq.Block $ \handle -> do
         Client.setName (handle :: SndSeq.T SndSeq.OutputMode) "midi-drone"
         putStrLn "Created sequencer."
         Port.withSimple handle "primary" (Port.caps [Port.capRead, Port.capSubsRead]) Port.typeMidiGeneric $ \port -> do
           user_port <- Connect.createTo handle port =<< Addr.parse handle (args !! 0)
           putStrLn "Created ports."
           -- let notes = [0 .. 127]
           let notes = [lower .. upper]
           -- let channels = [0 .. 15]
           sequence_ [playNote handle user_port channel n delay| n <- notes]
           getChar
           sequence_ [stopNote handle user_port channel n | n <- notes]
    )
  where
    playNote handle port channel n delay =
      do
        putStrLn $ "Channel " ++ show channel ++ " Note " ++ show n
        Event.outputDirect handle $ Event.forConnection port $ Event.NoteEv Event.NoteOn (Event.simpleNote (Event.Channel channel) (Event.Pitch n) Event.normalVelocity)
        threadDelay $ delay * 1000
    stopNote handle port channel n =
      Event.outputDirect handle $ Event.forConnection port $ Event.NoteEv Event.NoteOff (Event.simpleNote (Event.Channel channel) (Event.Pitch n) Event.normalVelocity)

-- Taken directly from the example
listPorts :: IO()
listPorts =
  do
    putStrLn " Port    Client name                      Port name"
    SndSeq.withDefault SndSeq.Block $ \h ->
      ClientInfo.queryLoop_ (h :: SndSeq.T SndSeq.OutputMode) $ \cinfo -> do
        client <- ClientInfo.getClient cinfo
        PortInfo.queryLoop_ h client $ \pinfo -> do
          join $ liftM4 (printf "%3d:%-3d  %-32.32s %-24.24s\n")
            (fmap (\(Client.Cons p) -> p) $ PortInfo.getClient pinfo)
            (fmap (\(Port.Cons p) -> p) $ PortInfo.getPort pinfo)
            (ClientInfo.getName cinfo)
            (PortInfo.getName pinfo)  
