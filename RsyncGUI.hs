{- Copyright (c) 2007 John Goerzen <jgoerzen@complete.org>
   Please see the COPYRIGHT file -}

module RsyncGUI where

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

data GUIParts = GUIParts {
    lfile :: Label,
    ltotal :: Label,
    mainwin :: Window,
    pbfile :: ProgressBar,
    pbtotal :: ProgressBar,
    messages :: TextView,
    messageswin :: ScrolledWindow,
    btdone :: Button}

initRsyncGUI :: IO () -> IO GUIParts
initRsyncGUI exitfunc = 
     do initGUI
        timeoutAddFull (yield >> return True)
                       priorityDefaultIdle 50
        gladefn <- getDataFileName "gtkrsync.glade"
        Just xml <- xmlNew gladefn
        -- Just xml <- xmlNew "gtkrsync.glade"

        window' <- xmlGetWidget xml castToWindow "mainwindow"
        onDestroy window' exitfunc

        pbfile' <- xmlGetWidget xml castToProgressBar "progressbarfile"
        pbtotal' <- xmlGetWidget xml castToProgressBar "progressbaroverall"
        lfile' <- xmlGetWidget xml castToLabel "labelfile"
        ltotal' <- xmlGetWidget xml castToLabel "labeloverall"
        messages' <- xmlGetWidget xml castToTextView "messages"
        messageswin' <- xmlGetWidget xml castToScrolledWindow "messageswindow"
        button' <- xmlGetWidget xml castToButton "donebutton"
        onClicked button' exitfunc

        let gui = GUIParts lfile' ltotal' window' pbfile' pbtotal' messages'
                  messageswin' button'
        
        forkIO mainGUI
        return gui

runGUI gui rsyncstream = 
    do streamWithMsgActions <- procmessages gui rsyncstream
       procstream gui streamWithMsgActions
       labelSetText (lfile gui) ""
       labelSetText (ltotal gui) "Sync process has finished"
       progressBarSetFraction (pbfile gui) 1.0
       progressBarSetText (pbfile gui) ""
       progressBarSetFraction (pbtotal gui) 1.0
       progressBarSetText (pbtotal gui) ""
       buttonSetLabel (btdone gui) "gtk-close"

exitApp = 
    do mainQuit
       exitWith ExitSuccess

procmessages gui stream = 
    do buf <- textViewGetBuffer (messages gui)
       iter <- textBufferGetEndIter buf
       mark <- textBufferCreateMark buf Nothing iter True
       -- tag <- textTagNew Nothing
       -- set tag [textTagFamily := "Monospace"]
       -- textBufferApplyTag buf tag iter iter
       return $ map (\x -> (procmsg gui buf mark x, snd x)) stream

procmsg gui buf mark (ltype, msg) =
    do end <- textBufferGetEndIter buf
       ipoint <- textBufferGetIterAtMark buf mark
       textBufferDelete buf ipoint end
       textBufferInsert buf ipoint ('\n' : msg)

       lines <- textBufferGetLineCount buf
       when (lines > 500) $ do
               start <- textBufferGetStartIter buf
               eol <- textBufferGetIterAtLine buf 1
               textBufferDelete buf start eol 

       -- scroll to the end of the buffer
       adj <- scrolledWindowGetVAdjustment (messageswin gui)
       upper <- adjustmentGetUpper adj
       adjustmentSetValue adj upper

       -- Update the iterator the new offset
       case ltype of
            HardLine -> do end <- textBufferGetEndIter buf
                           textBufferMoveMark buf mark end
            SoftLine -> return () -- leave the mark where it is
       return ()

procstream gui stream =
    do (totalfiles, remainingstream) <- procscanning gui stream
       progress <- newProgress "total" totalfiles
       mapM_ (procprogress gui progress) remainingstream

procscanning gui [] = return (0, [])
procscanning gui ((action,x):xs)
    | isSuffixOf "files..." x = 
        action 
        >> labelSetText (ltotal gui) ("Scanned " ++ (head (words x)) ++ " files")
        >> progressBarPulse (pbtotal gui)
        >> procscanning gui xs
    | isSuffixOf "files to consider" x =
        action
        >> labelSetText (ltotal gui) "" 
        >> progressBarSetFraction (pbtotal gui) 0.0
        >> return (read . head . words $ x, xs)
    | otherwise = action >> procscanning gui xs

procprogress gui progress (action, line)
    | progressl /= [] =
        do action
           case head progressl of
             [_, bytes, pct] -> 
               progressBarSetFraction (pbfile gui) ((read pct) / 100)
               >> progressBarSetText (pbfile gui) (pct ++ "%")
             x -> fail $ "Couldn't handle " ++ show x
           case tocheck of
             [] -> return ()
             [[_, thisfile, total]] ->
                 progressBarSetFraction (pbtotal gui) 
                    (1.0 - (ithisfile / itotal))
                 >> progressBarSetText (pbtotal gui)
                      ("File " ++ show (floor (itotal - ithisfile))
                       ++ " of " ++ total ++ " (" ++ show (intpct) ++ "%)")
                 >> setP progress (floor (itotal - ithisfile))
                 >> setetr
                 where itotal = read total 
                       ithisfile = read thisfile 
                       intpct = floor (100 * (1.0 - (ithisfile / itotal)))
                       setetr = do etr <- getETR progress
                                   labelSetText (ltotal gui) 
                                     ("ETA: " ++ renderSecs etr)
             x -> fail $ "Tocheck couldn't handle " ++ show x
    | otherwise =
        action >> labelSetText (lfile gui) line

    where progressl :: [[String]]
          progressl = line =~ "^ *([0-9]+) +([0-9]+)%" -- .+[0-9]+:[0-9]+:[0-9]+" =~ line
          tocheck = line =~ "xfer#[0-9]+, to-check=([0-9]+)/([0-9]+)"

oobError gui msg = 
    do dlg <- messageDialogNew (Just (mainwin gui)) [] MessageError ButtonsOk
              ("An error has been detected:\n\n" ++ msg ++ 
               "\n\nExpand the Messages area in the main window for details.")
       dialogRun dlg
       widgetDestroy dlg

