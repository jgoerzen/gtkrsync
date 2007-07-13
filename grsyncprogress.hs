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

data Progress = Progress {
    filebartext :: String,
    filebarlabel :: String,
    filebarfrac :: Maybe Double,
    totalbartext :: String,
    totalbarlabel :: String,
    totalbarfrac :: Maybe Double,
    finished :: Bool}
    deriving (Eq, Show)

initialprogress = (Progress "" "" (Just 0) "" "" Nothing False)

main = do
    hSetBuffering stdin (BlockBuffering Nothing)
    rsyncinput <- getContents
    let rsyncstream = customlines rsyncinput
    mv <- newMVar initialprogress
    forkIO (runGUI mv)
    procstream mv rsyncstream

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

runGUI mv = 
     do initGUI
        timeoutAddFull (yield >> return True)
                       priorityDefaultIdle 50
        -- gladefn <- getDataFileName "grsyncprogress.glade"
        -- FIXME: use this for release: Just xml <- xmlNew gladefn
        Just xml <- xmlNew "grsyncprogress.glade"

        window <- xmlGetWidget xml castToWindow "mainwindow"
        onDestroy window mainQuit

        pbfile <- xmlGetWidget xml castToProgressBar "progressbarfile"
        pbtotal <- xmlGetWidget xml castToProgressBar "progressbaroverall"
        lfile <- xmlGetWidget xml castToLabel "labelfile"
        ltotal <- xmlGetWidget xml castToLabel "labeloverall"
        
        firststatus <- readMVar mv
        forkIO (procData mv initialprogress pbfile pbtotal lfile ltotal)
        mainGUI

procData mv lastprogress pbfile pbtotal lfile ltotal = do
        threadDelay 1000000
        stat <- readMVar mv
        when (stat /= lastprogress) (paintgui stat)
        procData mv stat pbfile pbtotal lfile ltotal
    where paintgui stat = do
              progressBarSetText pbfile (filebartext stat)
              progressBarSetText pbtotal (totalbartext stat)
              case (filebarfrac stat) of
                   Nothing -> progressBarPulse pbfile
                   Just x -> progressBarSetFraction pbfile x
              case (totalbarfrac stat) of
                   Nothing -> progressBarPulse pbtotal
                   Just x -> progressBarSetFraction pbtotal x
              labelSetText lfile (filebarlabel stat)
              labelSetText ltotal (totalbarlabel stat)

procstream mv stream = 
    do (totalfiles, remainingstream) <- procscanning mv (map snd stream)
       mapM_ (procprogress mv totalfiles) remainingstream

tweak mv func =
    modifyMVar_ mv (\x -> return (func x))

procscanning mv [] = return (0, [])
procscanning mv (x:xs)
    | isSuffixOf "files..." x = 
        tweak mv (\y -> y { totalbarlabel = 
                                  "Scanned " ++ (head (words x)) ++ " files"})
        >> procscanning mv xs
    | isSuffixOf "files to consider" x =
        print "procscanning returning" >> 
        return (read (head (words x)), xs)
    | otherwise = print x >> procscanning mv xs

procprogress mv totalfiles line
    | progressl /= [] =
        do case head progressl of
             [_, bytes, pct] -> 
               tweak mv 
                 (\x -> x {filebarfrac = Just ((read pct) / 100),
                           filebartext = pct ++ "%"})
             x -> fail $ "Couldn't handle " ++ show x
           case tocheck of
             [] -> return ()
             [[_, thisfile, total]] ->
                 tweak mv
                 (\x -> x {totalbarfrac = Just ((read total) - (read thisfile) / (read total)),
                           totalbartext = "File " ++ thisfile ++ " of " 
                                          ++ total})
             x -> fail $ "Tocheck couldn't handle " ++ show x
    | otherwise =
        print progressl >>
        print line >> 
        tweak mv (\x -> x {filebarlabel = line})

    where progressl :: [[String]]
          progressl = line =~ "^ *([0-9]+) +([0-9]+)%" -- .+[0-9]+:[0-9]+:[0-9]+" =~ line
          tocheck = line =~ "xfer#[0-9]+, to-check=([0-9]+)/([0-9]+)"

