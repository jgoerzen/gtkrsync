{- Copyright (c) 2007 John Goerzen <jgoerzen@complete.org>
   Please see the COPYRIGHT file -}

import System.IO

data LineType = SoftLine | HardLine
    deriving (Eq, Read, Show)

type RsyncLine = (LineType, String)

main = do
    hSetBuffering stdin (BlockBuffering Nothing)
    rsyncinput <- getContents
    let rsyncstream = customlines rsyncinput
    mapM_ print rsyncstream

customlines :: String -> [RsyncLine]
customlines "" = []
customlines x = case xs of
                     [] -> [(HardLine, line)]
                     ('\n' : next) -> (HardLine, line) : customlines next
                     ('\r' : '\n' : next) -> (HardLine, line) : customlines next
                     ('\r' : next) -> (SoftLine, line) : customlines next
    where (line, xs) = break (`elem` "\n\r") x

