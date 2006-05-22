module MIME.RFC2045 where

-- Standard GHC Modules

import Data.List
import Text.ParserCombinators.Parsec hiding (token)

-- Local Modules

import MIME.MIME

-- * MIME-Version Header Field
-- RFC2822 § 4

data FieldError 
    = NoSuchField String
    | ValueParseError ParseError
      deriving Show

-- NOTE: perhaps the get* fuctions should take an optional default
-- paramater to return. Or, since in many cases there is a predefined
-- default, a Bool flag that says return default if not found.

-- | Get the MIME-Version -- if the field appears more than once, we use the first one
-- FIXME: needs to strip comments too.
-- THINK: should we differentiate between the field missing and the value being unparsable?
getMimeVersion :: General2822 a -> Either FieldError String
getMimeVersion (General2822 fields _) = 
    case lookup "MIME-Version" fields of
      Nothing -> Left (NoSuchField "MIME-Version")
      Just v ->
          case parse mimeVersion v v of
            Left e -> Left (ValueParseError e)
            Right ver -> Right ver
      where mimeVersion :: CharParser st String
            mimeVersion =
                do semanticCFWS
                   d1 <- digit
                   semanticCFWS
                   char '.'
                   semanticCFWS
                   d2 <- digit
                   semanticCFWS
                   return [d1,'.',d2]

getContentType :: General2822 a -> Either FieldError ContentType
getContentType (General2822 fields _) = 
    case lookup "Content-Type" fields of
      Nothing -> Left (NoSuchField "Content-Type")
      Just ctStr -> 
          case parse contentType ctStr ctStr of
            Left e -> Left (ValueParseError e)
            Right ct -> Right ct

-- getContentEncoding :: General2822 a -> Maybe 


-- * Syntax of the Content-Type Header Field
-- RFC2822 § 5.1

data ContentType 
    = ContentType (String, String) [Parameter]
      deriving (Show, Read, Eq)

-- |Parse Content-type
contentType :: CharParser st ContentType
contentType =
    do semanticCFWS
       t <- token
       semanticCFWS
       char '/'
       semanticCFWS
       st <- token
       semanticCFWS
       p <- many parameter
       return $ ContentType (t,st) p

type Parameter = (String, String)

parameter :: CharParser st Parameter
parameter =
    do char ';'
       semanticCFWS
       a <- token
       semanticCFWS
       char '='
       semanticCFWS
       v <- token <|> (quotedString False)
       semanticCFWS
       return (a,v)

-- |token: 1*<any (US-ASCII) CHAR except SPACE, CTLs, or tspecials>
token :: CharParser st String
token = many1 (alphaNum <|> oneOf "!#$%&'*+-^_`{|}~.")

-- * Content-Transfer-Encoding Syntax
-- RFC2045 § 6.1

getContentTransferEncoding :: General2822 a -> Either FieldError String
getContentTransferEncoding (General2822 fields _) =
    case lookup "Content-Transfer-Encoding" fields of
      Nothing -> Left (NoSuchField "Content-Transfer-Encoding")
      Just cteStr ->
          either (Left . ValueParseError) Right (parse contentTransferEncoding cteStr cteStr)
    where contentTransferEncoding =
              do semanticCFWS
                 t <- token
                 semanticCFWS
                 return t

