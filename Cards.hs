module Cards where

import JSON
import Control.Monad

data Card = Card { name     :: String,
                   manaCost :: String,
                   supertypes :: [String],
                   types    :: [String], 
                   subtypes :: [String],
                   edition  :: String, 
                   pt       :: String,
                   cmc      :: Int, 
                   text     :: String} deriving (Show, Read)

mapChars :: (Char -> Char) -> Card -> Card
mapChars f card = card { name = map f (name card),
                         text = map f (text card)}                     

-- functions to extract card data from json file

getEditions :: Value -> Maybe [String]
getEditions = fmap (map fst) . object 

getCardsEd :: String -> Value -> Maybe [Card]
getCardsEd ed val = do allData     <- object val
                       edition     <- lookup ed allData
                       editionData <- object edition
                       cards       <- lookup "cards" editionData
                       cardData    <- array cards
                       mapM (mkCard ed) cardData
 
getNameEd :: String -> Value -> Maybe String
getNameEd ed val = do allData <- object val
                      edition <- lookup ed allData
                      editionData <- object edition
                      getString "name" editionData

mkCard :: String -> Value -> Maybe Card
mkCard ed val =         do xs <- object val
                           name0       <- getString "name" xs
                           supertypes0 <- getStrings "supertypes" xs
                           types0      <- getStrings "types" xs
                           subtypes0   <- getStrings "subtypes" xs
                           power       <- getString "power" xs
                           toughness   <- getString "toughness" xs
                           let pt0 = case (power,toughness) of
                                       ("","") -> ""
                                       (p,t)   -> p++"/"++t
                           text0       <- getString "text" xs
                           manaCost0   <- getString "manaCost" xs
                           cmc0        <- getNumber "cmc" xs
                           return $ Card {name=name0, supertypes=supertypes0, types=types0, subtypes=subtypes0,
                                           pt=pt0, text=text0, manaCost=manaCost0, cmc=cmc0, edition=ed}


-- Error-safe getters for JSON fields
object :: Value -> Maybe [(String,Value)]
object (Object xs) = Just xs
object _           = Nothing

array :: Value -> Maybe [Value]
array (Array xs) = Just xs
array _          = Nothing

string :: Value -> Maybe String
string (String s) = Just s
string _          = Nothing

number :: Value -> Maybe Int
number (Number x) = Just $ truncate x --we don't do un-stuff anyways, and apart from that, there's no decimal points on magic cards.
number _          = Nothing

-- code for looking up fields of cards
getNumber :: String -> [(String,Value)] -> Maybe Int
getNumber key dict = do intData <- (lookup key dict)`mplus`(return (Number 0)) --Apparently, cards without the manaCost field in the mtgJSON library also have no cmc field (despite it being defined as 0 in that case by the rules)
                        number intData

getString :: String -> [(String,Value)] -> Maybe String
getString key dict = do stringData <- (lookup key dict)`mplus`(return (String ""))--if field is not present, we want "" rather than an error.
                        string stringData

getStrings :: String -> [(String,Value)] -> Maybe [String]
getStrings key dict = do listData   <- (lookup key dict)`mplus`(return (Array []))--if field is not present, we want [] rather than an error.
                         stringsData <- array listData
                         mapM string stringsData
                         

