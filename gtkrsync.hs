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

    readh <- fdToHandle readfd
    hSetBuffering readh (BlockBuffering Nothing)

    rsyncinput <- hGetContents readh
    let rsyncstream = customlines rsyncinput
    gui <- initRsyncGUI exitApp
    installHandler sigCHLD (Catch (chldHandler gui pid)) Nothing

    runGUI gui rsyncstream

childFunc args rsyncbin readfd writefd =
    do closeFd readfd
       dupTo writefd stdOutput
       dupTo writefd stdError
       closeFd writefd
       executeFile rsyncbin True args Nothing

chldHandler gui pid = 
    do ps <- getProcessStatus True False pid
       case ps of
            Just ps -> case ps of
                            Exited 0 -> return ()
                            x -> oobError gui ("rsync exited with unexpected error: " ++ show x)
            Nothing -> return ()

