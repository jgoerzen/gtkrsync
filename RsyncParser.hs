{- Copyright (c) 2007 John Goerzen <jgoerzen@complete.org>
   Please see the COPYRIGHT file -}

import Data.List

data LineType = SoftLine | HardLine
    deriving (Eq, Read, Show)

-- | A line of input.  The 'LineType' corresponds to the type of EOL
-- character that occured **BEFORE** the String.
type RsyncLine = (LineType, String)

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

