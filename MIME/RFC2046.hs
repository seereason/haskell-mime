-- |module for decoding multipart message into various media types
module MIME.RFC2046 where

import Text.ParserCombinators.Parsec

import MIME.MIME
import MIME.RFC2045
import qualified MIME.Base64 as Base64

data MultipartMessage
    = Part ContentType [Field] String
    | Mixed [Either String MultipartMessage]

instance Show MultipartMessage where
    show (Part (ContentType ("text","plain") _) _ str) = str ++ "\n"
    show (Part (ContentType (mtype,subType) _) _ str) = mtype ++"/"++ subType++"\n"
    show (Mixed parts) = "multipart/mixed\n" ++ concatMap show parts

-- |saveParts decodes all the parts of a multipart message and writes
-- them to the supplied FilePath
-- NOTE: incomplete.
saveParts :: FilePath -> MultipartMessage -> IO ()
saveParts dir (Part (ContentType ("image","jpeg") params) _ image) = 
    case lookup "name" params of
      Nothing -> putStrLn "Could not find file name"
      Just n -> writeFile (dir ++"/"++n) image
saveParts dir (Part (ContentType ("text","plain") params) _ text) = putStrLn text
saveParts dir (Mixed parts) = mapM_ (either print (saveParts dir)) parts

-- |
-- TODO.
decodeMsg :: (General2822 a) -> MultipartMessage
decodeMsg g2822 =
    let parts = getParts 
    in
      (Part (ContentType ("text", "plain") []) [] "nothing done yet")

-- |FIXME: a bunch of stuff in here is supposed to be case-insensitive
getParts :: String -> Either String MultipartMessage
getParts str =
    case parse general2822 str str of
      Left e -> Left (show e)
      Right g@(General2822 fields body) ->
          case getContentType g of
            Left e -> Left (show e)
            Right (ContentType ("multipart",subType) params)
                | subType == "mixed" ->
                    case lookup "boundary" params of
                      Nothing -> Left ("Unable to find boundary")
                      Just boundary ->
                          case parse (parts boundary) body body of
                            Left e -> Left (show e)
                            Right b -> Right (Mixed (map getParts b))
            Right ct -> 
                case getContentTransferEncoding g of
                  Left e -> Right (Part ct fields body)
                  Right enc
                      | enc == "base64" -> Right (Part ct fields (Base64.decode body))
                      | otherwise -> Right (Part ct fields body)
--                      | enc == "quoted-printable" -> Right (Part ct Base64.decode)

-- * MIME part parsers
-- These parser attempt to split a multipart MIME message into its
-- parts.

-- |FIXME: can this be done more efficiently
parts :: String -> CharParser st [String]
parts boundaryMarker =
    do parts <- many (try (part boundaryMarker))
       endBoundary boundaryMarker
       return parts

part :: String -> CharParser st String
part boundaryMarker =
    do preamble <- manyTill anyChar (try (startBoundary boundaryMarker))
       part <- manyTill anyChar (try (lookAhead (boundary boundaryMarker)))
       return part

boundary :: String -> CharParser st ()
boundary boundaryMarker =
    do ocrlf
       string $ "--" ++ boundaryMarker
       return ()
       
startBoundary :: String -> CharParser st ()
startBoundary boundaryMarker =
    do -- ocrlf -- commenting this out is a hack so that we don't miss the first part in a body
       string $ "--" ++ boundaryMarker
       semanticCFWS
       ocrlf
       return ()

endBoundary :: String -> CharParser st ()
endBoundary boundaryMarker =
    do ocrlf
       string $ "--" ++ boundaryMarker ++ "--"
       semanticCFWS
       ocrlf
       return ()
