import Cards
import JSON
import qualified Data.Set as S
import System.Directory
import System.IO
import Control.Monad
import qualified Data.IntMap as M

main = do b <- doesFileExist "AllSets.json"
          case b of
            False -> putStrLn "Failed to find file AllSets.json, make sure it is located in the current directory and rerun init."
            True -> do putStrLn "Parsing.."
                       s <- readFile "AllSets.json"
                       let result = parseJSON s
                       case result of
                         Nothing -> putStrLn "Syntax error in file."
                         Just val -> do putStrLn "Parsing finished."
                                        let eds = getEditions val
                                        case eds of
                                          Nothing -> putStrLn "Unexpected file structure."
                                          Just eds -> do prepFiles
                                                         putStrLn "Processing cards.."
                                                         processEditions eds val (M.fromList []) (S.fromList [])

prepFiles :: IO ()
prepFiles = do exists <- doesDirectoryExist "files"
               unless exists (createDirectory "files")

processEditions :: [String] -> Value -> M.IntMap Handle -> S.Set String -> IO ()
processEditions [] _ hs _              = do mapM (hFlush.snd) (M.toList hs)
                                            putStrLn "Processing finished." 
processEditions (ed:eds) val hs names | ed `elem` ["UNH","UGL"] = processEditions eds val hs names
processEditions (ed:eds) val hs names = case (getNameEd ed val, getCardsEd ed val) of
                                         (Just edname,Just cs) -> do putStrLn ("Now processing "++edname)
                                                                     (hs',names') <- processCards cs hs names
                                                                     processEditions eds val hs' names'
                                         _                     -> putStrLn ("Unexpected file structure.")

processCards :: [Card] -> M.IntMap Handle -> S.Set String -> IO (M.IntMap Handle, S.Set String)
processCards     [] hs names = return (hs,names)
processCards (c:cs) hs names = case isNoCreature c || (manaCost c == "" && name c /= "Dryad Arbor") || (name c) `S.member` names of
                                True  -> processCards cs hs names
                                False -> case M.lookup (cmc c) hs of
                                          Just h  -> do hPrint h c
                                                        processCards cs hs (S.insert (name c) names)
                                          Nothing -> do h <- openFile ("files/creatures"++(show (cmc c))) WriteMode
                                                        hPrint h c
                                                        processCards cs (M.insert (cmc c) h hs) (S.insert (name c) names)
                                            

isNoCreature :: Card -> Bool
isNoCreature c = not $ "Creature" `elem` (types c)


