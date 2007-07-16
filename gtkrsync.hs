{- Copyright (c) 2007 John Goerzen <jgoerzen@complete.org>
   Please see the COPYRIGHT file -}

import System.IO
import Control.Concurrent.MVar
import RsyncParser
import RsyncGUI
import System.Environment
import System.Process
import System.Posix.IO
import System.Posix.Signals

main = do
    args <- getArgs
    rsyncbin <- catch (getEnv "RSYNC") (\_ -> "rsync")

    (readfd, writefd) <- createPipe
    pid <- forkProcess (childFunc args rsyncbin readfd writefd)
    closeFd writefd
    hasExited <- newMVar False

    readh <- fdToHandle readfd
    hSetBuffering readh (BlockBuffering Nothing)

    rsyncinput <- hGetContents readh
    let rsyncstream = customlines rsyncinput
    gui <- initRsyncGUI (exitButton pid hasExited)
    installHandler sigCHLD (Catch (chldHandler gui pid hasExited)) Nothing

    runGUI gui rsyncstream

exitButton pid mv = withMVar mv $ \hasexited ->
    if hasexited
       then exitApp
       else do -- Cancel signal handler since we don't want notification to
               -- user of exit due to user's own action
               installHandler sigCHLD Default Nothing
               -- No need to update the MVar here since there won't be
               -- anything else to read it.  Besides, doing so would cause
               -- deadlock anyway.
               signalProcess sigKILL pid
               exitApp

childFunc args rsyncbin readfd writefd =
    do closeFd readfd
       dupTo writefd stdOutput
       dupTo writefd stdError
       closeFd writefd
       executeFile rsyncbin True args Nothing

chldHandler gui pid mv = 
    do ps <- getProcessStatus True False pid
       case ps of
            Just ps -> do swapMVar mv True
                          case ps of
                            Exited 0 -> return ()
                            x -> oobError gui ("rsync exited with unexpected error: " ++ show x)
            Nothing -> return ()

