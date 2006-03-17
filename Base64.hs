module Base64 where


import Data.Bits
import Data.Char
import qualified Data.Map as Map
import Data.Maybe
import Data.Word

base64Chars = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ ['+','/']

encodeMap = Map.fromList (zip [0..63] base64Chars)
decodeMap = Map.fromList (zip base64Chars [0..63])

encodeC :: Word -> Char
encodeC w = fromJust $ Map.lookup w encodeMap

decodeC :: Char -> Word
decodeC c = fromJust $ Map.lookup c decodeMap

-- | 
charToWord :: Char -> Word
charToWord c = fromIntegral (ord c)

wordToChar :: Word -> Char
wordToChar w = chr (fromIntegral w)

maskBits :: Int -> Word -> Word
maskBits nBits i = ((1 `shiftL` nBits) - 1) .&. i

cShiftR :: Char -> Int -> Word
cShiftR c n = (charToWord c) `shiftR` n

cShiftL :: Char -> Int -> Word
cShiftL c n = (charToWord c) `shiftL` n

char3ToIndex4 :: (Char, Char, Char) -> (Word, Word, Word, Word)
char3ToIndex4 (a,b,c) =
    ( (maskBits 6 (a `cShiftR` 2))
    , (maskBits 6 (a `cShiftL` 4)) .|. (maskBits 4 (b `cShiftR` 4))
    , (maskBits 6 (b `cShiftL` 2)) .|. (maskBits 2 (c `cShiftR` 6))
    , (maskBits 6 (charToWord c))
    )

wShiftR :: Word -> Int -> Char
wShiftR w n = wordToChar (w `shiftR` n)

wShiftL :: Word -> Int -> Char
wShiftL w n = wordToChar (w `shiftL` n)

index4ToChar3 :: (Word, Word, Word, Word) -> (Char, Char, Char)
index4ToChar3 (w1, w2, w3, w4) =
    ( wordToChar (((maskBits 6 w1) `shiftL` 2) .|. (maskBits 2 (w2 `shiftR` 4)))
    , wordToChar (((maskBits 4 w2) `shiftL` 4) .|. (maskBits 4 (w3 `shiftR` 2)))
    , wordToChar (((maskBits 2 w3) `shiftL` 6) .|. (maskBits 6 (w4)))
    )

encode :: String -> String
encode [] = []
encode [a] = let (c1, c2,_,_) = char3ToIndex4 (a,'\0','\0') in [encodeC c1, encodeC c2,'=','=']
encode [a,b] = let (c1, c2, c3,_) = char3ToIndex4 (a, b,'\0') in [encodeC c1, encodeC c2, encodeC c3,'=']
encode (a:b:c:rest) = let (c1,c2,c3,c4) = char3ToIndex4 (a,b,c) in
                      ((encodeC c1):(encodeC c2):(encodeC c3):(encodeC c4):(encode rest))

decode :: String -> String 
decode = decode' . filter (`elem` ('=':base64Chars))

-- |Assumes that all whitespace, etc has been stripped
decode' :: String -> String
decode' [] = []
decode' [a,b,'=','='] = let (c1,'\0','\0') = index4ToChar3 (decodeC a, decodeC b,0,0) in [c1]
decode' [a,b,c,'='] = let (c1,c2,'\0') = index4ToChar3 (decodeC a,decodeC b, decodeC c,0) in [c1, c2]
decode' (a:b:c:d:rest) = 
    let (c1,c2,c3) = index4ToChar3 (decodeC a, decodeC b, decodeC c, decodeC d) in 
    (c1 : c2 : c3 : (decode rest))
