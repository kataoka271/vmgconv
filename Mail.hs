module Mail (
      Mail (..)
    , mailDate
    , mailTo
    , mailFrom
    , parseMail
    , multiparts
    , unwrapMultipart
    , showMail
  ) where

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import Control.Applicative hiding (many, optional, (<|>))
import Text.Parsec
import Text.Parsec.ByteString
import Data.Time.Format (parseTime)
import Data.Time (UTCTime)
import System.Locale (defaultTimeLocale)


data Mail = Mail { -- MIME message format
      smsHeaders :: [(String, String)]
    , mailHeaders :: [(String, String)]
    , mailContents :: B.ByteString
  } deriving (Eq, Show)


mailDate :: Mail -> Maybe UTCTime
mailDate (Mail _ headers _) =
    parseTime defaultTimeLocale "%a, %e %b %Y %T %z" =<< lookup "Date" headers

mailTo :: Mail -> Maybe String
mailTo (Mail _ headers _) = lookup "To" headers

mailFrom :: Mail -> Maybe String
mailFrom (Mail _ headers _) = lookup "From" headers

parseMail :: B.ByteString -> Maybe Mail
parseMail s = case parse p_message "" s of
                   Left _ -> Nothing
                   Right m -> Just m

p_message :: GenParser t st Mail
p_message = Mail <$> many smsHeader <*> p_headers <*> contents
  where smsHeader = liftA2 (,) (try (many upper <* char '='))
                               (many (upper <|> oneOf "-_")) <* string "\r\n"
        contents = C.pack <$> many anyChar

p_headers :: GenParser t st [(String, String)]
p_headers = header `manyTill` string "\r\n"
  where header = liftA2 (,) name (char ':' *> many (oneOf " \t") *> value)
        name = liftA2 (:) letter (many (letter <|> digit <|> oneOf "-_"))
        value = liftA2 (++) (many (noneOf "\r\n") <* string "\r\n")
                            (trailer <|> pure [])
        trailer = liftA2 (:) (' ' <$ many1 (oneOf " \t")) value

p_boundary :: GenParser t st String
p_boundary = string "multipart/" *> many1 (noneOf ";") *> char ';' *>
             spaces *> string "boundary=" *>
             between (char '"') (char '"') (many1 (noneOf "\""))

p_multipart :: String -> GenParser t st [String]
p_multipart brd = string ("--" ++ brd) *>
                  many1 (string "\r\n" *> content) <*
                  string ("--\r\n")
  where content = anyChar `manyTill` try (string ("\r\n--" ++ brd))

multipart :: Mail -> [Mail]
multipart (Mail _ headers body) = do
    t <- case lookup "Content-Type" headers of
              Nothing -> []
              Just r -> [r]
    b <- case parse p_boundary "" (C.pack t) of
              Left _ -> []
              Right r -> [r]
    m <- case parse (p_multipart b) "" body of
              Left _ -> []
              Right r -> [r]
    case mapM (parse p_message "" . C.pack) m of
         Left _ -> []
         Right rs -> rs

multiparts :: Mail -> [Mail]
multiparts m = case multipart m of
                    [] -> [m]
                    ms -> m : concatMap multiparts ms

unwrapMultipart :: Mail -> Mail
unwrapMultipart m =
    let ms = multipart m
    in if length ms == 1
          then m { mailHeaders = remove (mailHeaders m) ++ mailHeaders (head ms)
                 , mailContents = mailContents (head ms) }
          else m
  where remove = filter ((/= "Content-Type") . fst)

showMail :: Mail -> B.ByteString
showMail m =
    B.concat (map showPair (smsHeaders m)) `B.append`
    B.concat (map showPair (mailHeaders m)) `B.append`
    crlf `B.append`
    mailContents m
  where crlf = C.pack "\r\n"
        showPair (k, v) = C.pack $ k ++ ": " ++ v ++ "\r\n"
