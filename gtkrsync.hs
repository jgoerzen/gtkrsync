{- Copyright (c) 2007 John Goerzen <jgoerzen@complete.org>
   Please see the COPYRIGHT file -}

import System.IO
import Control.Concurrent.MVar
import RsyncParser
import RsyncGUI
import System.Environment
import System.Process

main = do
    args <- getArgs
    rsyncbin <- catch (getEnv "RSYNC") (\_ -> "rsync")
    (inp, out, err, ph) <- runInteractiveProcess rsyncbin args Nothing Nothing
    hClose inp

    hSetBuffering out (BlockBuffering Nothing)
    hSetBuffering err (BlockBuffering Nothing)

    rsyncinput <- getContents
    let rsyncstream = customlines rsyncinput
    gui <- initRsyncGUI exitApp

    runGUI gui rsyncstream

