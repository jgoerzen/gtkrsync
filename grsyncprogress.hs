{- Copyright (c) 2007 John Goerzen <jgoerzen@complete.org>
   Please see the COPYRIGHT file -}

import System.IO
import Control.Concurrent.MVar
import Control.Concurrent
import Data.List
import Text.Regex.Posix

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

main = do
    hSetBuffering stdin (BlockBuffering Nothing)
    rsyncinput <- getContents
    let rsyncstream = customlines HardLine rsyncinput
    mv <- newMVar (Progress "" "" (Just 0) "" "" Nothing False)
    forkIO (runGUI mv)
    procstream mv rsyncstream

customlines :: LineType -> String -> [RsyncLine]
customlines _ "" = []
customlines lt x = 
    case xs of
         [] -> [(lt, line)]
         ('\n' : next) -> (lt, line) : customlines HardLine next
         ('\n' : '\r' : next) -> (lt, line) : customlines HardLine next
         ('\r' : '\n' : next) -> (lt, line) : customlines HardLine next
         ('\r' : next) -> (lt, line) : customlines SoftLine next
    where (line, xs) = break (`elem` "\n\r") x

runGUI mv = 
     do stat <- takeMVar mv
        print stat
        threadDelay 1000000
        runGUI mv

procstream mv stream = 
    do (totalfiles, remaningstream) <- procscanning mv (map snd stream)
       mapM_ (procprogress mv totalfiles) remainingstream

procscanning mv [] = return (0, [])
procscanning mv (x:xs)
    | isSuffixOf "files..." x = 
        modifyMVar_ mv (\x -> x { totalbar = 
                                  "Scanned " ++ (head (words x)) ++ " files"})
        >> procscanning mv xs
    | isSuffixOf "files to consider" x =
        return (read (head (words x)), xs)
    | otherwise = procscanning mv xs

procprogress mv totalfiles line
    | progressl = 
        case progressl of
         [bytes, pct] -> 
           modifyMVar_ mv 
             (\x -> x {filebarfrac = Just ((read pct) / 100),
                       filebartest = pct ++ "%"})
         x -> fail $ "Couldn't handle " ++ x
    | tocheck =
        case tocheck of
             [_, thisfile, total] ->
                 modifyMVar_ mv
                 (\x -> x {totalbarfrac = Just ((read thisfile) / (read total)),
                           totalbartext = "File " thisfile ++ " of " ++ total})
             x -> fail $ "Tocheck couldn't handle " ++ x
    | otherwise =
        modifyMVar_ mv (\x -> x {filebarlabel = line})

    where progressl = "^ *([0-9]+) +([0-9]+)%.+[0-9]+:[0-9]+:[0-9]+" =~ line
          tocheck = "xfer#[0-9]+, to-check=([0-9]+)/([0-9]+)" =~ line

