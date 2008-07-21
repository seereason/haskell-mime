{- This module is where I am working on a new API design for creating
and modifying MIME messages. It is not yet in use.

-}
import Control.Monad
import Control.Monad.Error
import Data.List
import Data.Time


-- * Message Data Types

-- |Headers
data To		= To      [Mailbox] deriving Show
data From	= From    Mailbox   deriving Show
data Date	= Date    ZonedTime deriving Show
data Subject	= Subject String    deriving Show
data Keywords	= Keywords [String] deriving Show

-- |Supporting types
data Mailbox    = Mailbox String deriving Show

-- |Message body
data Body	= Body    String    deriving Show

class Header h where
    toHeader :: h -> RawHeader -- perhaps toHeader should be a normal polymorphic function, and this should be an escape routine
    fromHeader :: (Monad m) => RawHeader -> m h
    headerStr :: h -> String

instance Header To where
    toHeader = undefined
    fromHeader = undefined
    headerStr _ = "To"

instance Header From where
    toHeader = undefined
    fromHeader = undefined
    headerStr _ = "From"
                     
instance Header Date where
    toHeader = undefined
    fromHeader = undefined
    headerStr _ = "Date"

-- |TODO: add proper escaping
instance Header Subject  where
    toHeader (Subject s) = ((headerStr (undefined :: Subject)), s)
    fromHeader (h,v) = return $ Subject v
    headerStr _ = "Subject"

-- |TODO: add proper escaping
instance Header Keywords  where
    toHeader (Keywords w) = ((headerStr (undefined :: Keywords)), concat (intersperse ", " w))
    fromHeader = undefined
    headerStr _ = "Keywords"

-- * Filter types

-- should raw header be escaped or unescaped?  we want escaped if we
-- are about to print it, but unescaped it we are looking stuff up. If
-- we are looking stuff up though, we might want structured
-- fields. Like above. We also would like to only decode the escaped
-- stuff once, but we don't want to force decoding everything all the
-- time.

type RawHeader = (String, String)

-- |add a header, leaving any current instance intact
-- useful for headers that can appear any number of times
-- does not include headers that can appear only 0 or 1 times
class (Header h) => AddHeader h

-- * Filter Instances

-- |Headers that can appear multiple times
instance AddHeader Keywords

type HFilter = ([RawHeader] -> [RawHeader])

-- |set a header, replacing all existing occurences 
setHeader :: (Header h) => h -> HFilter
setHeader h headers = (toHeader h) : (removeHeaders h headers)

-- |add a header, leaving current occurences intact
-- new header will appear first
addHeader :: (AddHeader h) => h -> HFilter
addHeader h headers = (toHeader h) : headers

-- |remove all headers of a specific type
-- probably need a variation that removes a specific header
removeHeaders :: forall h. (Header h) => h -> HFilter
removeHeaders _ = filter (\(n,_) -> n /= (headerStr (undefined :: h)))

-- * Searching

-- |use a the list monad for multiple results
lookupHeader :: forall m h. (MonadPlus m, Header h) => [RawHeader] -> m h
lookupHeader rawHeaders =
    case filter ((==) header . fst) rawHeaders of
      [] -> fail $ "Could not find header: " ++ header
      headers -> msum $ map fromHeader headers
    where
      header :: String
      header = headerStr (undefined :: h)

-- * Experimental infix header combinators

infixr 0 .+.
infixr 0 .*.

(.+.) :: (Header h) => h -> HFilter
(.+.) h = (setHeader h)

(.*.) :: (AddHeader h) => h -> HFilter
(.*.) h = (addHeader h)

empty :: [RawHeader]
empty =  []

exampleHeaders =
    ( (setHeader (Subject "whee")) .
      (setHeader (Subject "bork")).
      (addHeader (Keywords ["baz", "bar", "bam"])) .
      (addHeader (Keywords ["zip", "zap", "zop"]))
    )

-- it does not seem entirely obvious that the operator *after* the
-- header is the one that selects whether a header will appear once or
-- multiple times.
exampleHeaders2 :: [RawHeader]
exampleHeaders2 =
    ((Subject "whee") .+. 
     (Subject "bork") .+.
     (Keywords ["baz", "bar", "bam"]) .+.
     (Keywords ["zip", "zap", "zop"]) .+.
     empty
    )

-- * HFilter helpers

subject = setHeader . Subject
keywords = addHeader . Keywords

exampleHeaders3 :: [RawHeader] -> [RawHeader]
exampleHeaders3 =
    ((subject "whee") .
     (subject "bork") .
     (keywords ["baz", "bar", "bam"]) .
     (keywords ["zip", "zap", "zop"]))

-- * JUNK

{- 


data HCons h t = HCons h t deriving Show
data HNil = HNil deriving Show

infixr .*. 
h .*. t = HCons h t


-- This looks pretty, but the type-error messages do not
message someAddr myAddr currTime =
    ( To [someAddr] .*.
      From myAddr .*.
      Date currTime .*.
      Subject "Test Message" .*.
      Body "I like cheese."
    )

-- This sort of hackery requires overlapping/incoherent type
-- instances, and produces nasty error messages
class HasHeader h m where
    hasHeader :: h -> m -> Bool

instance HasHeader h (HCons h t) where
    hasHeader _ _ = True

instance (HasHeader h t) => HasHeader h (HCons e t) where
    hasHeader _ _ = True
    
{-
class ValidMessage m where
                 
sendMessage :: (ValidMessage m) => m -> String
sendMessage = 
-}

-- parse :: String -> m
-- parse

-}