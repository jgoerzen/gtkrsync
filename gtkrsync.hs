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
import System.Posix.Process
import System.Exit

main = do
    args <- getArgs
    rsyncbin <- catch (getEnv "RSYNC") (\_ -> return "rsync")

    (readfd, writefd) <- createPipe
    pid <- forkProcess (childFunc args rsyncbin readfd writefd)
    closeFd writefd
    hasExited <- newMVar False

    readh <- fdToHandle readfd
    hSetBuffering readh (BlockBuffering Nothing)

    rsyncinput <- hGetContents readh
    let rsyncstream = customlines rsyncinput
    exitmv <- newMVar Nothing
    gui <- initRsyncGUI (exitButton pid hasExited exitmv)
    installHandler sigCHLD (Catch (chldHandler gui pid hasExited exitmv)) Nothing

    -- Check to see if we died before installing the handler
    ps <- getProcessStatus False False pid
    case ps of
         Nothing -> return ()
         Just x -> chldPs gui x hasExited exitmv

    runGUI gui rsyncstream exitmv

exitButton pid mv exitmv = withMVar mv $ \hasexited ->
    if hasexited
       then exitApp exitmv
       else do -- Cancel signal handler since we don't want notification to
               -- user of exit due to user's own action
               installHandler sigCHLD Default Nothing
               -- No need to update the MVar here since there won't be
               -- anything else to read it.  Besides, doing so would cause
               -- deadlock anyway.
               signalProcess sigKILL pid
               exitApp exitmv

childFunc args rsyncbin readfd writefd =
    do closeFd readfd
       dupTo writefd stdOutput
       dupTo writefd stdError
       closeFd writefd
       executeFile rsyncbin True args Nothing

chldHandler gui pid mv exitmv = 
    do ps <- getProcessStatus True False pid
       case ps of
            Just ps -> chldPs gui ps mv exitmv
            Nothing -> return ()

chldPs gui ps mv exitmv =
    do installHandler sigCHLD Default Nothing
       swapMVar mv True
       case ps of
         Exited ExitSuccess -> return ()
         Exited x -> do oobError gui ("rsync exited with unexpected error: " ++ show x)
                        swapMVar exitmv (Just x) >> return ()
         x -> do oobError gui ("rsync exited with unexpected condition: " ++ show x)
                 swapMVar exitmv (Just (ExitFailure 255)) >> return ()
