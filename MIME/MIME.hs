module MIME.MIME where

import Control.Monad
import Data.List hiding (group)
import Text.ParserCombinators.Parsec

-- Is there anything type-specific about a MIME document? Perhaps the
-- internal structure of the stuff should be reflected in the type? So
-- that you can only pass a MIME document to a function than can
-- handle it? That might be nice, but you would have to know all the
-- types/subtypes in advance -- which is not feasible. Therefore
-- anything that takes something of type MIME, should be able to
-- 'handle' it somehow.

-- If you only read a field, then you should write it out the same. If
-- you modify it, then you can rebreak it however you want. Should you
-- preserve comments?

-- type ContentType = (String, String) [String]
{-
data MIME
    = MIME
      { mimeVersion :: String -- RFC2045 § 4
      , contentType :: ContentType
      , contentTransferEncoding
      }
-}

-- |This is a very generalized 2822-style format. No validity checking
-- is done.  the 'state' is a phantom type which can be used to mark
-- the data as having some sort of property -- such as 2822 validity
-- or something of that nature. If users are not allowed to directly
-- construct General2822 data-types this could work... This allows us
-- to, hopefully, preserve the formatting of a document when applying
-- transformations on it -- which is important if parts of it are gpg
-- signed -- for example.
data General2822 state
    = General2822 [Field] Body
      deriving Show
{-
fields :: General2822 a -> [Field]
fields (General2822 fields _) = fields

body :: General2822 a -> [Field]
body (General2822 _ body) = body
-}
type Body 
    = String

-- |NOTE: Perhaps add a phantom-type indicating if the string is
-- currently folded or not
type FWSString = [String]

type Field = (String, String)

-- * Extra parser combinators

pappend :: (CharParser st String) -> (CharParser st String) -> (CharParser st String)
pappend p1 p2 =
    do r1 <- p1
       r2 <- p2
       return (r1 ++ r2)

pconcat :: (CharParser st [[a]]) -> (CharParser st [a])
pconcat p =
    do r <- p
       return  $ concat r

-- |a?(ba)*b?
-- NI: alternate

-- |(a(ba)*b?|b(ab)*a?)
-- Alternate between two parsers. Return atleast one thing
-- Could this maybe be done using chain?
alternate1 :: (CharParser st a) -> (CharParser st a) -> (CharParser st [a])
alternate1 p1 p2 = 
    (try (alternate1' p1 p2)) <|> (try (alternate1' p2 p1))
    where
      alternate1' p1 p2 =
          do a <- (count 1 p1)
             ba <- many (try (p2 >>= \b -> p1 >>= \a -> return [b,a]))
             b <- option [] (try (count 1 p2))
             return $ a ++ (concat ba) ++ b

-- * A very liberal 822-style parser

general2822 :: CharParser st (General2822 ())
general2822 =
    do fields <- many genericField
       ocrlf -- is this really optional ?
       body <- many anyChar
       return $ General2822 fields body

genericField :: CharParser st Field
genericField =
    do fieldName <- ftext
       char ':'
       fieldValue <- textWithFWS
       return $ (fieldName, fieldValue)
       
textWithFWS :: CharParser st String
textWithFWS =
    do first <- try $ do u <- many (utext' <|> wsp)
                         lf <- ocrlf
                         return $ u ++ lf
       rest <- many $ try $ do w <- wsp  -- must start with whitespace
                               u <- many1 (utext' <|> wsp)
                               lf <- ocrlf
                               return $ [w] ++ u ++ lf
       return (concat (first:rest))

unfoldFWS :: String -> String
unfoldFWS [] = []
unfoldFWS ('\n':rest) = unfoldFWS rest
unfoldFWS ('\r':'\n':rest) = unfoldFWS rest
unfoldFWS (c : rest) = c : unfoldFWS rest

-- |Remove leading and trailing white space
-- TODO: smoosh multiple whitespaces into one ?
cleanWS :: String -> String
cleanWS = reverse . dropWhile (==' ') . reverse . dropWhile (==' ')

canonicalWS = unfoldFWS . cleanWS

-- * Optional fields
-- RFC2822 § 3.6.8

optionalField :: CharParser st Field
optionalField =
    do fieldName <- ftext
       char ':'
       ut <- unstructuredText
       ocrlf
       return (fieldName, ut)

ftext :: CharParser st String
ftext = many1 fchar

fchar :: CharParser st Char
fchar = oneOf $ ['\33'..'\57'] ++ ['\59'..'\126']

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

ocrlf :: CharParser st String
ocrlf =
    try (crlf <|> (lf >>= \l -> return [l]))


-- * 3.2.1 Primitive Tokens

noWsCtl :: CharParser st Char
noWsCtl =
    oneOf (['\1'..'\8'] ++ ['\11','\12'] ++ ['\14'..'\31'] ++ ['\127'])

text :: CharParser st Char
text =
    oneOf (['\1'..'\8'] ++ ['\11','\12'] ++ ['\14'..'\127']) <|> obsText

specials :: CharParser st Char
specials = oneOf "()<>[]:;@\\,.\""

-- * Quoted Characters
-- RFC2045 § 3.2.2

quotedPair :: CharParser st Char
quotedPair = 
    do char '\\'
       text

-- * Folding white space and comments
-- RFC2822 § 3.2.3

-- FIXME: can this be simplified ?
fws :: CharParser st String
fws =
    (try $ do w <- many wsp
              lb <- ocrlf <|> return ""
              w' <- many1 wsp
              return $ w ++ lb ++ w')
    <|>
    (try $ do lb <- ocrlf <|> return ""
              w' <- many1 wsp
              return $ lb ++ w')
    

-- |Optional fws -- return empty string if there was no whitespace
ofws :: CharParser st String
ofws = option [] fws
    

-- |ctext is US-ASCII \\ ['(',')','\']
ctext :: CharParser st Char
ctext = noWsCtl <|> (oneOf $ ['\33'..'\39'] ++ ['\42'..'\91'] ++ ['\93'..'\126'])

ccontent :: CharParser st String
ccontent = (count 1 ctext) <|> (count 1 quotedPair) <|> comment

comment :: CharParser st String
comment =
    do char '('
       c <- many (ofws >>= \ws -> ccontent >>= \c -> return (ws ++ c))
       ws <- ofws
       char ')'
       return $ "(" ++ (concat c) ++ ws ++ ")"

cfws :: CharParser st String
cfws = (try (((many1 (try (ofws `pappend` comment))) >>= return . concat) `pappend` ofws)) <|> fws

ocfws :: CharParser st String
ocfws = option [] (try cfws)

-- * Atom
-- RFC2822 § 3.2.4

atext :: CharParser st Char
atext = alphaNum <|> oneOf "!#$%&'*+-/=?^_`{|}~"

atom :: CharParser st String
atom = 
    do ws1 <- ocfws
       c <- many1 atext
       ws2 <- ocfws
       return $ (ws1 ++ c ++ ws2)

dotAtom :: CharParser st String
dotAtom =
    do ws1 <- ocfws
       a <- dotAtomText
       ws2 <- ocfws
       return $ ws1 ++ a ++ ws2

dotAtomText :: CharParser st String
dotAtomText =
    do a <- many1 atext
       as <- many (string "." `pappend` many1 atext)
       return $ a ++ (concat as)

-- * Quoted Strings
-- RFC2822 § 3.2.5

-- |US-ASCI \\ ['\']
qtext :: CharParser st Char
qtext = noWsCtl <|> (oneOf $ ('\33': ['\35'..'\91']) ++ ['\93'..'\126'])

qcontent :: CharParser st Char
qcontent = qtext <|> quotedPair

quotedString :: Bool -> CharParser st String
quotedString preserve =
    do ws1 <- ocfws
       char '"'
       strs <- many1 (ofws `pappend` (count 1 qcontent))
       ws2 <- ofws
       char '"'
       ws3 <- ocfws
       if preserve
         then return $ (ws1 ++ "\"" ++ (concat strs) ++ ws2 ++ "\"" ++ ws3)
         else return (concat strs)

-- * Miscellaneous tokens
-- RFC2822 § 3.2.6

word :: CharParser st String
word = atom <|> (quotedString True)

-- | FIXME: obs-phrase
phrase :: CharParser st String
phrase = many1 word >>= return . concat

utext :: CharParser st Char
utext = noWsCtl <|> oneOf ['\33'..'\126']

utext' :: CharParser st Char
utext' = noWsCtl <|> oneOf ['\33'..'\255']

-- |unstructured = *([FWS] utext) [FWS]
unstructuredText :: CharParser st String
unstructuredText = option [] (pconcat (alternate1 (many1 (utext' <|> wsp)) fws))
{- 
    do iniFWS <- option [] foldedws
       rest <- sepBy utextStr foldedws
       return $ iniFWS ++ rest
    where utextStr = many1 (utext <|> wsp)
          foldedws = try $ do ocrlf
                              lookAhead wsp
                              return $ [""]
-}

-- * Address Specification
-- RFC2822 § 3.4

addressList :: CharParser st [String]
addressList = sepBy address (char ',')

address :: CharParser st String
address = mailbox {- <|> group -}

mailbox :: CharParser st String
mailbox = (try nameAddr) <|> addrSpec

nameAddr :: CharParser st String
nameAddr =
    do dn <- displayName
       aa <- angleAddr
       return $ dn ++ aa

angleAddr :: CharParser st String
angleAddr =
    do ws1 <- ocfws
       char '<'
       as <- addrSpec
       char '>'
       ws2 <- ocfws
       return $ ws1 ++ "<" ++ as ++ ">" ++ ws2
{-
group :: CharParser st String
group =
    do dn <- displayName
       char ':'
       g <- many (mailboxList <|> cfws)
       char ';'
       ws <- ocfws
       return $ dn ++ ":" ++ (concat g) ++ ";" ++ ws
-}
displayName :: CharParser st String
displayName = phrase

mailboxList :: CharParser st [String]
mailboxList = sepBy1 mailbox (char ',')

-- * Addr-spec specification
-- RFC2822 § 3.4.1

addrSpec :: CharParser st String
addrSpec =
    do lp <- localPart
       char '@'
       d <- domain
       return $ (lp ++ "@" ++ d)

-- |FIXME: obs-local-part
localPart :: CharParser st String
localPart = dotAtom <|> (quotedString True)

-- |FIXME: obs-domain
domain :: CharParser st String
domain = dotAtom <|> domainLiteral

domainLiteral :: CharParser st String
domainLiteral =
    do ws1 <- ocfws
       char '['
       c <- many (ofws `pappend` (count 1 dcontent))
       ws2 <- ofws
       char ']'
       ws3 <- ocfws
       return $ ws1 ++ "[" ++ (concat c) ++ ws2 ++ "]" ++ ws3

dcontent :: CharParser st Char
dcontent = dtext <|> quotedPair

-- | US-ASCII \\ ['[',']','\']
dtext :: CharParser st Char
dtext = noWsCtl <|> (oneOf $ ['\33'..'\90'] ++ ['\94'..'\126'])
       

-- * Miscellaneous obsolete tokens
-- RFC2045 § 4.1

obsText :: CharParser st Char
obsText = fail "obsText not implemented yet"

-- * Core Rules
-- RFC2234 § 6.1

wsp :: CharParser st Char
wsp = char ' ' <|> char '\t'

-- * Obsolete Folding White Space
-- RFC2234 § 6.1

-- |Return fws with crlf stripped out
obsFWS :: CharParser st String
obsFWS = 
    do h <- many1 wsp
       tl <- chainl (try (ocrlf >> many1 wsp)) (return $ (++)) []
       return $ h ++ tl

-- | Return just the whitespace with folding and comments removed
semanticCFWS :: CharParser st String
semanticCFWS = liftM concat $ sepBy1 (option [] ((try fws) <|> obsFWS)) comment
