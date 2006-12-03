-- |module for decoding quoted-printable text (see RFC1521)
module MIME.QuotedPrintable (decode, quotedPrintable) where

import Control.Monad
import Data.Char
import Numeric (readHex)
import Text.ParserCombinators.Parsec

import MIME.MIME

decode :: String -> Either ParseError [String]
decode str = either Left Right $ parse quotedPrintable str str

quotedPrintable :: CharParser st [String]
quotedPrintable =
    do h <- qpLine
       tl <- many (try ( ocrlf >> qpLine))
       return (h:tl)

qpLine :: CharParser st String
qpLine =
    do s <- many (try (qpSegment >>= \sg -> transportPadding >> ocrlf >> return sg))
       last <- qpPart >>= \p -> transportPadding >> return p
       return $ (concat s) ++ last

qpPart :: CharParser st String
qpPart = qpSection

qpSegment = 
    do s <- qpSection
       many wsp
       char '='
       return s

qpSection = liftM concat $ many $ do w <- many wsp
                                     t <- many1 ptext
                                     return (w ++ t)

ptext = (try hexOctet) <|> safeChar

safeChar :: CharParser st Char
safeChar = oneOf $ ['\33'..'\60'] ++ ['\62'..'\126']

hexOctet :: CharParser st Char
hexOctet =
    do char '='
       d1 <- digit <|> oneOf ['A'..'F']
       d2 <- digit <|> oneOf ['A'..'F']
       return . chr . fst . head . readHex $ [d1,d2]

transportPadding :: CharParser st String
transportPadding = many wsp
