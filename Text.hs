module Text where

import System.IO        
import Prelude hiding (Left,Right)
import Data.Monoid
import Printer

data Character = Small Char | Big Char deriving (Show)--possibly additional symbols, like mana symbols...
data Line      = Line [Character] [Character] deriving (Show)
data Text      = Text [Line] deriving (Show)


char :: Character -> Char
char (Big c) = c
char (Small c) = c

linefeed :: Line
linefeed = Line [] []

instance Monoid Line where
 mempty = Line [] []
 (Line cl1 cr1) `mappend` (Line cl2 cr2) = Line (cl1++cl2) (cr1++cr2)


class ToLine a where
 toLine:: a -> Line

instance ToLine Line where
 toLine = id

instance ToLine Char where
 toLine c = Line [Small c] []

instance ToLine Character where
 toLine c = Line [c] []

instance ToLine a => ToLine [a] where
 toLine = mconcat . map toLine

left :: (ToLine a) => a -> Line
left x = case toLine x of
          Line cs1 cs2 -> Line (cs1 ++ cs2) []

right :: (ToLine a) => a -> Line
right x = case toLine x of
           Line cs1 cs2 -> Line [] (cs1 ++ cs2)

big       :: (ToLine a) => a -> Line
big x = case toLine x of
         Line cs1 cs2 -> Line (map (Big . char) cs1) (map (Big . char) cs2)

small    :: (ToLine a) => a -> Line
small x = case toLine x of
           Line cs1 cs2 -> Line (map (Small . char) cs1) (map (Small . char) cs2)

leftright :: (ToLine a, ToLine b) => a -> b -> Line
leftright x y = (left x) `mappend` (right y)

width :: Character -> Int
width (Small _) = 1
width (Big _)   = 2

mode :: Character -> Mode
mode (Big _) = BigMode
mode (Small _) = SmallMode

showCharsMode :: Printer -> Mode -> [Character] -> String
showCharsMode p m []     = changeMode p m SmallMode
showCharsMode p m (c:cs) = (changeMode p m (mode c)) ++ [char c] ++ showCharsMode p (mode c) cs

showChars :: Printer -> [Character] -> String
showChars p = showCharsMode p SmallMode

showLine :: Printer -> Line -> String
showLine p (Line cs [])  = (showChars p cs) ++ "\n"
showLine p (Line [] cs)  = let w = lineWidth p in
                           let n = sum (map width cs) `mod` w in
                           let offset = if n == 0 then 0 else w-n in
                            (replicate offset ' ') ++ (showChars p cs) ++ "\n"
showLine p (Line cs1 cs2) = let w = lineWidth p in
                            let n = sum (map width (cs1++cs2)) `mod` w in
                            let offset = w-n in
                             (showChars p cs1) ++ (replicate offset ' ') ++ (showChars p cs2) ++ "\n"

showText :: Printer -> Text -> String
showText p (Text xs) = concat . (map (showLine p)) $ xs

printText :: Printer -> Text -> IO ()
printText p = put p . showText p
