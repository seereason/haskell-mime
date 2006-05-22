module MIME.RFC2046 where

import Text.ParserCombinators.Parsec

import MIME.MIME
import MIME.RFC2045
import qualified MIME.Base64 as Base64

data MultipartMessage
    = Part ContentType String
    | Mixed [Either String MultipartMessage]

instance Show MultipartMessage where
    show (Part (ContentType ("text","plain") _) str) = str ++ "\n"
    show (Part (ContentType (mtype,subType) _) str) = mtype ++"/"++ subType++"\n"
    show (Mixed parts) = "multipart/mixed\n" ++ concatMap show parts

saveParts :: FilePath -> MultipartMessage -> IO ()
saveParts dir (Part (ContentType ("image","jpeg") params) image) = 
    case lookup "name" params of
      Nothing -> putStrLn "Could not find file name"
      Just n -> writeFile (dir ++"/"++n) image
saveParts dir (Part (ContentType ("text","plain") params) text) = putStrLn text
saveParts dir (Mixed parts) = mapM_ (either print (saveParts dir)) parts

decodeMsg :: (General2822 a) -> MultipartMessage
decodeMsg g2822 =
    let parts = getParts 
    in
      (Part (ContentType ("text", "plain") []) "nothing done yet")

-- |FIXME: a bunch of stuff in here is supposed to be case-insensitive
getParts :: String -> Either String MultipartMessage
getParts str =
    case parse general2822 str str of
      Left e -> Left (show e)
      Right g@(General2822 _ body) ->
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
                  Left e -> Right (Part ct body)
                  Right enc
                      | enc == "base64" -> Right (Part ct (Base64.decode body))
                      | otherwise -> Right (Part ct body)
--                      | enc == "quoted-printable" -> Right (Part ct Base64.decode)

-- |FIXME: can this be done for efficiently
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
startBoundary boundary =
    do ocrlf
       string $ "--" ++ boundary
       semanticCFWS
       ocrlf
       return ()

endBoundary :: String -> CharParser st ()
endBoundary boundary =
    do ocrlf
       string $ "--" ++ boundary ++ "--"
       semanticCFWS
       ocrlf
       return ()
