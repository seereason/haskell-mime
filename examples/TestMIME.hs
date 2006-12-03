module TestMIME where

import Text.ParserCombinators.Parsec

import MIME
import RFC2045
import RFC2046

testP p str = print (parse p str str) 

strFWS1 = " \n  "
testFWS1 = testP fws strFWS1

strFWS2 = " "
testFWS2 = testP fws strFWS2

strUST = "\n This is a\n test of \n folding  \n  whitespace \n "
testUST = testP unstructuredText strUST

strUST2 = "This is a\n test of \n folding  \n  whitespace \n "
testUST2 = testP unstructuredText strUST2

general2822Str =
    ("Header: this is a header\n"++
     "HeaderFWS: this one has\n folding white space\n"++
     "\n"++
     "the body is here\n"
     )

testg2822 = testP general2822 general2822Str 

emailAddr1 = "jeremy@n-heptane.com"
testEmailAddr1 = testP addressList emailAddr1

emailAddr2 = " Jeremy Shaw <jeremy.shaw@example.com>, jeremy@example.com (i rock)"
testEmailAddr2 = testP addressList emailAddr2

debControl :: CharParser st [[Field]]
debControl = 
    do f <- sepEndBy1 (many1 genericField) ocrlf <?> "main body"
--       many anyChar
       return f

testDeb fp =
    do c <- readFile fp
       return $ parse debControl fp c 
       
testMail fp =
    do c <- readFile fp
       return $ parse general2822 fp c

contentType1 = "text (i like cheese)/plain; zoink=\"whee\"; zap = dard"
testContentType1 = testP contentType contentType1