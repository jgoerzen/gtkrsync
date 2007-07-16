{- Copyright (c) 2007 John Goerzen <jgoerzen@complete.org>
   Please see the COPYRIGHT file -}

import System.IO
import Control.Concurrent.MVar
import Control.Concurrent
import Data.List
import Text.Regex.Posix
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Control.Monad
import System.Exit
import Data.Progress.Tracker
import System.Time.Utils
import RsyncParser
import RsyncGUI

main = do
    hSetBuffering stdin (BlockBuffering Nothing)
    rsyncinput <- getContents
    let rsyncstream = customlines rsyncinput
    runGUI rsyncstream

