{- Copyright (c) 2007 John Goerzen <jgoerzen@complete.org>
   Please see the COPYRIGHT file -}

import System.IO
import Control.Concurrent.MVar
import RsyncParser
import RsyncGUI
import System.Exit

main = do
    hSetBuffering stdin (BlockBuffering Nothing)
    rsyncinput <- getContents
    let rsyncstream = customlines rsyncinput
    exitmv <- newMVar Nothing
    gui <- initRsyncGUI (exitApp exitmv)
    runGUI gui rsyncstream exitmv

