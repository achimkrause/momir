module Printer where

import System.IO        
import Data.Char

data Printer = Printer {setMode :: Mode -> String, lineWidth :: Int, put :: String -> IO (), handleUnicode :: Char -> Char}
data Mode = BigMode | SmallMode deriving (Eq)


pos58path :: String
pos58path = "/dev/usb/lp0"

changeMode :: Printer -> Mode -> Mode -> String
changeMode p m1 m2 = if m1 == m2 then "" else setMode p m2

modes :: String -> String -> (Mode -> String)
modes s1 _  SmallMode = s1
modes _  s2 BigMode   = s2


pos58 :: Printer 
pos58 = Printer {setMode = modes "\27\20" "\27\14", 
               lineWidth = 32, 
                     put = p,
           handleUnicode = h} where
                     p s = do {h<-openFile pos58path WriteMode
                              ;hSetBuffering h NoBuffering
                              ;hSetBinaryMode h True
                              ;hPutStr h s
                              ;hClose h}
                     h c = case lookup (ord c) unicodeTable of
                                Nothing -> c 
                                Just c' -> c'
unicodeTable :: [(Int,Char)] --several unicode chars on the printer's charset.
unicodeTable = [(8212, '-'), 
                (8211, '-'),
                (8722, '-'),
                (8216, '\''), 
                (8217, '\''),
                (226, '\131'),
                (228, '\132'),
                (245, 'o'),
                (225, '\160'),
                (237, '\161'),
                (250, '\163'),
                (251, '\150'),
                (233, '\130'),
                (198, '\146'),
                (234, '\136'),
                (252, '\129'),
                (246, '\148'),
                (196, '\142'),
                (224, '\133'),
                (232, '\138'),
                (230, '\145'),
                (176, '\248'),
                (178, '\253')]
                                                       
                            
stdout_printer :: Printer
stdout_printer = Printer {setMode = modes "" "", lineWidth = 32, put = putStr, handleUnicode = id}
