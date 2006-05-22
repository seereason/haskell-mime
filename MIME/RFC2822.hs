module MIME.RFC2822 where

import Text.ParserCombinators.Parsec

-- |carriage return @\\r@.
cr :: CharParser st Char
cr = char '\r' <?> "carriage return"

-- |linefeed @\\n@.
lf :: CharParser st Char
lf = char '\n' <?> "linefeed"

-- |linefeed @\\n@ as a string.
lfs :: CharParser st String
lfs = string "\n" <?> "linefeed"

crlf :: CharParser st String
crlf =
    do c <- cr
       l <- lf
       return [c,l]

-- * 3.2.1 Primitive Tokens

noWsCtl :: CharParser st Char
noWsCtl =
    oneOf (['\1'..'\8'] ++ ['\11','\12'] ++ ['\14'..'\31'] ++ ['\127'])

text :: CharParser st Char
text =
    oneOf (['\1'..'\8'] ++ ['\11','\12'] ++ ['\14'..'\127']) <|> obsText

specials :: CharParser st Char
specials = oneOf "()<>[]:;@\\,.\""

-- * 3.2.2 Quoted Characters

quotedPair :: CharParser st Char
quotedPair = 
    do char '\\'
       text

-- * 3.2.3 Folding white space and comments


obsText :: CharParser st Char
obsText = fail "obsText not implemented yet"
    