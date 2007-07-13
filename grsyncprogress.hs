{- Copyright (c) 2007 John Goerzen <jgoerzen@complete.org>
   Please see the COPYRIGHT file -}

import System.IO

data LineType = SoftLine | HardLine
    deriving (Eq, Read, Show)

type RsyncLine = (LineType, String)

main = do
    hSetBuffering stdin (BlockBuffering Nothing)
    rsyncinput <- getContents
    let rsyncstream = customlines HardLine rsyncinput
    mapM_ print rsyncstream

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

