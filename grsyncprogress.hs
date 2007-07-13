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

data LineType = SoftLine | HardLine
    deriving (Eq, Read, Show)

-- | A line of input.  The 'LineType' corresponds to the type of EOL
-- character that occured **BEFORE** the String.
type RsyncLine = (LineType, String)

data GUIParts = GUIParts {
    lfile :: Label,
    ltotal :: Label,
    mainwin :: Window,
    pbfile :: ProgressBar,
    pbtotal :: ProgressBar,
    messages :: TextView,
    messageswin :: ScrolledWindow}

main = do
    hSetBuffering stdin (BlockBuffering Nothing)
    rsyncinput <- getContents
    let rsyncstream = customlines rsyncinput
    runGUI rsyncstream

customlines :: String -> [RsyncLine]
customlines "" = []
customlines x = 
    case xs of
         [] -> [(HardLine, line)]
         -- ('\n' : '\r' : next) -> (HardLine, line) : customlines next
         ('\n' : next) -> (HardLine, line) : customlines next
         -- ('\r' : '\n' : next) -> (HardLine, line) : customlines next
         ('\r' : next) -> (SoftLine, line) : customlines next
    where (line, xs) = break (`elem` "\n\r") x

runGUI rsyncstream = 
     do initGUI
        timeoutAddFull (yield >> return True)
                       priorityDefaultIdle 50
        -- gladefn <- getDataFileName "grsyncprogress.glade"
        -- FIXME: use this for release: Just xml <- xmlNew gladefn
        Just xml <- xmlNew "grsyncprogress.glade"

        window' <- xmlGetWidget xml castToWindow "mainwindow"
        onDestroy window' mainQuit

        pbfile' <- xmlGetWidget xml castToProgressBar "progressbarfile"
        pbtotal' <- xmlGetWidget xml castToProgressBar "progressbaroverall"
        lfile' <- xmlGetWidget xml castToLabel "labelfile"
        ltotal' <- xmlGetWidget xml castToLabel "labeloverall"
        messages' <- xmlGetWidget xml castToTextView "messages"
        messageswin' <- xmlGetWidget xml castToScrolledWindow "messageswindow"


        let gui = GUIParts lfile' ltotal' window' pbfile' pbtotal' messages'
                  messageswin'
        
        forkIO mainGUI
        streamWithMsgActions <- procmessages gui rsyncstream
        procstream gui streamWithMsgActions

procmessages gui stream = 
    do buf <- textViewGetBuffer (messages gui)
       iter <- textBufferGetEndIter buf
       mark <- textBufferCreateMark buf Nothing iter True
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
    do remainingstream <- procscanning gui stream
       mapM_ (procprogress gui) remainingstream

procscanning gui [] = return []
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
        >> return xs
    | otherwise = action >> procscanning gui xs

procprogress gui (action, line)
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
                 where itotal = read total 
                       ithisfile = read thisfile 
                       intpct = floor (100 * (1.0 - (ithisfile / itotal)))
             x -> fail $ "Tocheck couldn't handle " ++ show x
    | otherwise =
        action >> labelSetText (lfile gui) line

    where progressl :: [[String]]
          progressl = line =~ "^ *([0-9]+) +([0-9]+)%" -- .+[0-9]+:[0-9]+:[0-9]+" =~ line
          tocheck = line =~ "xfer#[0-9]+, to-check=([0-9]+)/([0-9]+)"

