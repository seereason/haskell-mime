-- |module for decoding quoted-printable text (see RFC1521)
module Text.MIME.Codec.QuotedPrintable (encode, decode, quotedPrintable) where

import Control.Monad
import Data.Char
import Numeric (readHex)
import Text.ParserCombinators.Parsec

import Data.Bits ((.&.), shiftR)
import Data.Char (ord, toUpper, intToDigit)

import Text.MIME.Parse.MIME

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

-- * Encoder

{- 
RFC2045 
RFC1521 (obsolete)
-}


{- Cases to Handle:

+ space or tab appears immediately before \\n and pos is less than 75.
+ line is longer than 76 characters (not including \\r\\n)
+ character 75 or 76 needs to be escaped
+ 
-}

-- imported from mime-string, Codec.MIME.String.QuotedPrintable
-- Copyright Ian Lynagh, 2005, 2007

encode :: String -> String
encode = enc 0 . lines

-- The Int is the number of characters on this line so far
-- 76 is the maximum we can have no one line, and 3 is the most
-- generated for 1 input char (but we also need space for a trailing
-- '=' for a soft line break).
enc :: Int -> [String] -> String
enc _ [] = ""
enc _ [[]] = ""
enc _ ([]:ls) = '\n':enc 0 ls
enc n ls | n > 72 = '=':'\n':enc 0 ls
enc n ((c:cs):ls)
 | (33 <= o && o <= 126 && o /= 61) ||
   (not (null cs) && (o == 9 || o == 32))  = c:enc (n+1) (cs:ls)
 | otherwise                               = '=':x1:x2:enc (n+3) (cs:ls)
    where o = ord c
          x1 = toUpper $ intToDigit (o `shiftR` 4)
          x2 = toUpper $ intToDigit (o .&. 0xF)

