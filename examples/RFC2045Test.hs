import Test.HUnit.Base
import Test.HUnit.Text
import Text.ParserCombinators.Parsec

import MIME
import RFC2045
import Base64

mk822 :: String -> String -> General2822 ()
mk822 field value =
    General2822 [(field,value)] []

mimeVersionTest str =
    TestCase (assertEqual str "1.0" (either (\_ -> []) id (getMimeVersion (mk822 "MIME-Version" str))))

mimeVersionTests =
    TestList $ map mimeVersionTest [ "1.0"
                                   , " 1.0"
                                   , "1.0 (produced by MetaSend Vx.x)"
                                   , "(produced by MetaSend Vx.x) 1.0"
                                   , "1.(produced by MetaSend Vx.x)0"
                                   ]

contentTypeTest str =
    TestCase (assertEqual str (Just (ContentType ("text","plain") [("charset","us-ascii")])) (either (\_ -> Nothing) (\r -> Just r) (getContentType (mk822 "Content-Type" str))))

contentTypeTests =
    TestList $ map contentTypeTest [ " text/plain; charset=us-ascii (Plain text)"
                                   , " text/plain; charset=\"us-ascii\""
                                   ]

contentTransferEncodingTest (res, str) =
    TestCase (assertEqual str (Just res) (either (\_ -> Nothing) Just (getContentTransferEncoding (mk822 "Content-Transfer-Encoding" str))))

contentTransferEncodingTests =
    TestList $ map contentTransferEncodingTest [ ("7bit", "7bit")
                                               , ("binary", " binary")
                                               , ("base64", " base64 (super-encoder 2000)")
                                               ]
    
tests =
    do runTestTT mimeVersionTests
       runTestTT contentTypeTests
       runTestTT contentTransferEncodingTests