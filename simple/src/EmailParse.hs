module EmailParse where

import Text.ParserCombinators.Parsec
import Network.URL
import Control.Monad.IO.Class
import Lib

parseEmail :: GenParser Char st DirectionData
parseEmail = do
  moo <- (parseMode <|> return "walking")
  starting <- parseFrom
  ending <- (parseTo <|> return home)
  moo <- (parseMode <|> return moo)
  return $ DD starting ending moo

parseFrom :: GenParser Char st String
parseFrom = (try (string ":from ") <|>
            try (string ":f ") <|>
            string "") >> parseAddress

parseTo :: GenParser Char st String
parseTo = (try (string ":dest ")  >> parseAddress) <|>
          (try (string ":d ") >> parseAddress) <|>
          (try (string ":to ") >> parseAddress)

parseMode :: GenParser Char st String
parseMode = (((try (string ":walk ") <|> try (string ":w ")) >> return "walking") <|>
              ((try (string ":transit ") <|> try (string ":train ") <|> try (string ":t ")) >> return "transit") <|>
              ((try (string ":drive ")) >> return "driving"))

parseAddress :: GenParser Char st String
parseAddress = many1 $ noneOf "=:\n"

data DirectionData = DD { start :: String
                        , end :: String
                        , mode :: String
                        } deriving Show
  
emailToURL :: String -> Maybe URL
emailToURL s = case (parse parseEmail "" s) of
  Left err -> Nothing
  Right dd -> Just $ dDtoURL dd

dDtoURL :: DirectionData -> URL
dDtoURL (DD s e m) = addOrigin s $ addDest e $ addMode m $ addKey directionKey directionURL

emailAndResponse :: (MonadIO m) => String -> m String
emailAndResponse e = case (emailToURL e) of
   Nothing -> pure "Malformed query, text :h for help"
   Just u -> showResponse $ getDirections u

    
