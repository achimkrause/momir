import Printer
import Cards
import System.Random
import System.IO
import System.Directory
import Data.List
import Data.Maybe
import Control.Monad (unless)
import Text
import Printer
import qualified Data.IntMap as M





main = do b <- doesDirectoryExist "files"
          case b of
           False -> putStrLn "Directory \"files\" not found. You need to run init first."
           True  -> do maps <- load
                       gen <- getStdGen
                       b <- doesFileExist pos58path
                       hSetBuffering stdout NoBuffering
                       if b then game pos58 maps gen
                            else game stdout_printer maps gen

game :: Printer -> (M.IntMap (M.IntMap Card)) -> StdGen -> IO ()
game p maps gen = do putStr ">"
                     s <- getLine
                     case (s, reads s :: [(Int,String)]) of
                      ("",_) -> game p maps gen
                      ("q",_)-> return ()
                      (_,[(n,[])]) -> case M.lookup n maps of
                                           Nothing -> do putStrLn "No creature of that mana cost"
                                                         game p maps gen
                                           Just cards -> do let l = M.size cards
                                                            let (i,gen') = randomR (0,l-1) gen
                                                            let c = fromJust $ M.lookup i cards 
                                                            printText p (cardToText (mapChars (handleUnicode p) c))
                                                            game p maps gen'
                      (_,[]) -> do putStrLn "Syntax error"
                                   game p maps gen


cardToText :: Card -> Text
cardToText c = Text [leftright (name c) (manaCost c),
                     linefeed,
                     linefeed,
                     linefeed,
                     leftright (intercalate " " ((supertypes c) ++ (types c) ++ ["-"] ++ (subtypes c))) (edition c),
                     linefeed,
                     left (text c),
                     linefeed,
                     right . big $ (pt c ++ " "),
                     linefeed,
                     linefeed,
                     linefeed,
                     linefeed,
                     linefeed]

fileNameToCmc :: FilePath -> Maybe Int
fileNameToCmc str = do str' <- eat "creatures" str 
                       case reads str' of
                        [(n,[])] -> return n
                        _      -> Nothing

eat :: String -> String -> Maybe String
eat [] s = Just s
eat _ [] = Nothing
eat (c:cs) (c':cs') = if c==c' then eat cs cs' else Nothing


load :: IO (M.IntMap (M.IntMap Card))
load = do files <- getDirectoryContents "files"
          xs    <- mapM (\f -> maybe 
                           (unless (f `elem` [".",".."]) (putStrLn ("Warning: Ignoring file \""++f++"\"")) >> return [])
                           (\x -> do {s     <- readFile ("files/"++f)
                                     ;let cards = map read . lines $ s
                                     ;return [(x,M.fromList (zip [0..] cards))]})
                           (fileNameToCmc f)
                        ) files
          return (M.fromList (concat xs))
