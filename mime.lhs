types of email:

plain rfc2822
single-part MIME document
multipart MIME document
encapsulated message
signed

Desires:

Be able to process and add parts messages without fully decoding the
whole message. However, we do require some level of look ahead. For
example, you may have to may have to look through all the header
fields to find if the MIME-Version field is present or not.

Be able to manipulate MIME messages with out losing comments, etc.

Be able to generate messages.

\section{Extensible vs. Homogeneous}

There is a trade-off to consider. We can make the data-type be
extensible so that we can later extend the library to handle
additional RFCs. However, this will likely result in different
messages having different types -- or we have to use some sort of
dynamic typing system.

The other option is to pick a uniform data type that can represent all
the types of messages we want to handle, ranging from simple non-MIME
RFC822 messages, to fancy multipart MIME messages. This allows us to
create a homogenous list of messages, e.g. [Message]. It also results
in simplier type signatures and a simpler API.

Since the MIME system is not being extended much these days, it seems
better to pick a simple, uniform data that encapsulates everthing.

A MIME message must have a MIME-Version header field. Currently (and
probably forever) the only version is 1.0. However, the field can
contain comments, which we would like to preserve.

We would also like to preserve the order of the headers, since this
can be useful information -- for example the headers inserted by the
different mail relays.

For MIME messages, there are certain headers that are required or
commonly accessed -- so it would be nice to have those fields built-in
to the data type. However, that makes it difficult to maintain the
order of the header fields. Rather than store that 'cached' data in
the type, perhaps it should be the responsiblity of the user to cache
the fields they care about. This keeps the interface simple and
uniform -- and does not add caching requirements to programs that do
not even use those fields.

We also want to be able to continue in the face of errors when
appropriate -- but also report errors.


The ContentType and ContentEncoding are found it the headers, and
describe the body. In our type, should we keep them in the header
part, or the body part ? It is valid for them to appear in all media
types. 

\begin{code}

import Data.ByteString.Char8

type Name = ByteString
type Value = ByteString
type Line = Int
type Col = Int
type Pos = (FilePath, Line, Col)
type Field = (Name, Value)
type Headers = [Field]
data Part = Part Headers Body 

\end{code}

\begin{code}

type Boundary = String
type Preamble = ByteString
type Epilogue = ByteString
data Body
    = Discrete Bool ByteString  -- ^ encoded/decoded, data
    | Multipart Preamble [Part] Epilogue -- boundary is part of the the content-type
    | Message Part

data MultipartSubtype 
    = Mixed
    | Alternative
    | Digest
    | Parallel
    | Other String
      deriving Show
{-
data MessageSubtype
    = RFC2822
    | Partial
      deriving Show

instance Show MultipartMessage where
    show (Part (ContentType ("text","plain") _) _ str) = str ++ "\n"
    show (Part (ContentType (mtype,subType) _) _ str) = mtype ++"/"++ subType++"\n"
    show (Mixed parts) = "multipart/mixed\n" ++ concatMap show parts
-}

\end{code}

Perhaps we need a form that deals with already encoded data ? Though,
we should often already have a complete part in that case.

\begin{code}

data ContentEncoding = ContentEncoding deriving Show
data ContentType = ContentType deriving Show

createDiscretePart :: ContentType -> ContentEncoding -> Headers -> ByteString -> Part
createDiscretePart contentType contentEncoding additionalHeaders thedata =
    Part ((pack "Content-Type", pack $ show contentType) : (pack "Content-Encoding", pack $ show contentEncoding) : additionalHeaders) (Discrete True (encode contentEncoding thedata))

-- ContentType must include the boundary marker; How do we ensure that
-- the boundary marker does not appear in the body? We can not check
-- that until after it has been encoded. And, what do we do in the
-- case that it does appear in the body? Note that if we allow the
-- contents to be in memory an encoded or decoded state -- then we
-- obviously must do some processing to create the mime document
-- anyway.

createMultiPart :: ContentType -> Preamble -> [Part] -> Epilogue -> Part createMultiPart
createMultiPart = undefined

encode = undefined



\end{code}