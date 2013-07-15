module Vmsg (
      Vmsg (..)
    , IrmcType (..)
    , parseVmsg
  ) where

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import qualified Data.Map as M
import Control.Applicative hiding (many, optional, (<|>))
import Text.Parsec
import Text.Parsec.ByteString


data Vmsg = Vmsg { -- vMessage format
      vmsgFolderName :: Maybe String
    , vmsgIrmcType :: Maybe IrmcType
    , vmsgContents :: B.ByteString
  } deriving (Eq, Show)

data IrmcType = INET | SMS | Unknown String
  deriving (Eq, Show)

type Env = M.Map String String

parseVmsg :: B.ByteString -> [Vmsg]
parseVmsg s = case parse (vmsg M.empty) "" s of
                   Left _ -> []
                   Right m -> m

vmsg :: Env -> GenParser st t [Vmsg]
vmsg env = vbgn >>= vmsg' env

vmsg' :: Env -> String -> GenParser st t [Vmsg]
vmsg' env name = choice [
        [] <$ vend name
      , (:) <$> vbody env <*> vmsg' env name
      , (++) <$> vmsg env <*> vmsg' env name
      , vpair env >>= \env' -> vmsg' env' name
    ]

vbgn :: GenParser st t String
vbgn = try (string "BEGIN:") *> p_name <* string "\r\n"

vend :: String -> GenParser st t String
vend name = try (string "END:") *> string name <* string "\r\n"

vbody :: Env -> GenParser st t Vmsg
vbody env = do
    contents <- vbodyContents
    return $ Vmsg {
          vmsgFolderName = M.lookup "X-FJ-FOLDERNAME" env
        , vmsgIrmcType = irmcType `fmap` M.lookup "X-IRMC-TYPE" env
        , vmsgContents = C.pack contents
      }

vbodyContents :: GenParser st t String
vbodyContents = try (string "BEGIN:VBODY\r\n") *>
    manyTill anyChar (try $ string "END:VBODY\r\n")

irmcType :: String -> IrmcType
irmcType s = case s of
                  "INET" -> INET
                  "SMS" -> SMS
                  _ -> Unknown s

vpair :: Env -> GenParser st t Env
vpair env = M.insert <$> p_name <*> (p_param *> p_value) <*> pure env

p_name :: GenParser st t String
p_name = (:) <$> letter <*> many (digit <|> letter <|> oneOf "_-")

p_param :: GenParser st t [String]
p_param = many (char ';' *> many1 (noneOf ":;")) <* char ':'

p_value :: GenParser st t String
p_value = many (noneOf "\r\n") <* string "\r\n"
