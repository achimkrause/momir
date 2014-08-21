module JSON where
import Text.ParserCombinators.Parsec hiding ((<|>))
import Control.Applicative hiding (many)
import System.Directory

data Value = String String 
           | Number Double  
           | Bool Bool
           | Null
           | Object [(String,Value)]
           | Array [Value] deriving (Show)

parseJSON :: String -> Maybe Value
parseJSON s = case parse parseValue "" s of
                Left _ -> Nothing
                Right val -> Just val

parseValue :: CharParser () Value
parseValue = choice [String <$> parseString, 
                     Number <$> parseNumber, 
                     Object <$> parseObject, 
                     Array <$> parseArray, 
                     Bool <$> parseBool, 
                     Null <$ string "null"<* whitespace] 

parseNumber :: CharParser () Double                     
parseNumber =  do s <- getInput
                  case reads s of
                    []       -> empty
                    (n,s'):_ -> n <$ setInput s' <* whitespace

parseBool :: CharParser () Bool
parseBool = (True <$ string "true" <|> False <$ string "false")<* whitespace

parseString :: CharParser () String
parseString = (between (char '\"') (char '\"') (many parseChar)) <* whitespace

parseChar :: CharParser () Char
parseChar = (noneOf "\"\\") <|> (char '\\' *> choice ['\n' <$ char 'n', '\\' <$ char '\\', '\"' <$ char '\"']) 

parseArray :: CharParser () [Value]
parseArray = between (char '[' <* whitespace) (char ']' <* whitespace)
               (sepBy parseValue (char ',' <* whitespace))

parseObject :: CharParser () [(String,Value)]
parseObject = between (char '{' <* whitespace) (char '}' <* whitespace)
                 (sepBy ((,) <$> parseString <* char ':' <* whitespace <*> parseValue) (char ',' <* whitespace))

whitespace :: CharParser () String
whitespace = many (oneOf " \t\n")
