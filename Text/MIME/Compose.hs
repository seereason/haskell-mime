-- |Compose a MIME message
module Text.MIME.Compose where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception (evaluate)

import Data.Char (isDigit,ord)
import Data.List
import Data.Time

import System.Exit
import System.Locale
import System.Process
import System.IO


import Text.PrettyPrint.HughesPJ

import Text.MIME.Type

{-

TODO: how to do ensure certains fields do not appear more than once.

-} 


{-

1. Focus on building the best possible data-types/functions for
   constructing messages. Later we can update them for other things
   (parsing, reformating, etc).

2. If we build the message from the inside out, then we do not have to
   worry about the boundary tags getting invalidated. (Do we?)

-}

{-
Required Fields:

 MIME-Version: 1.0
 Content-Type: type/sub-type
 Content-Transfer-Encoding: (7bit | 8bit | binary | quoted-printable | base64 | ietf-token | x-token)

The Content-Type and Content-Transfer-Encoding are dictated by the
MIMEBody, so those values should come from that element.

The only required fields for rfc2822 are:

  originator date field
  originator address field(s)

We probably also want:

  Subject:
  To:
  Message-ID:

-}

-- * pretty-printers

ppMailbox :: Mailbox -> String
ppMailbox (AddrSpec localPart domainPart) =
    (bool id encodeQuotedString isDotAtom localPart) ++ ('@' : (bool id encodeDomainLiteral isDotAtom domainPart))

ppMailboxes :: [Mailbox] -> String
ppMailboxes = concat . intersperse ", " . map ppMailbox

ppAddress :: Address -> String
ppAddress (MailboxAddress mb) = ppMailbox mb

ppAddresses :: [Address] -> String
ppAddresses = concat . intersperse ", " . map ppAddress

data LineBreakStyle 
    = LF
    | CRLF
      deriving (Read, Show, Eq)

ppMessage :: LineBreakStyle -> Message String -> String
ppMessage lbs (RFC2822 headers body) =
    let lines = (concatMap ppHeader headers) ++ (blankLine : body)
    in
      case lbs of
        LF   -> (foldr terminateLF "" lines)
        CRLF -> (foldr terminateLF "" lines)
    where
      terminateLF l   = (l ++) . ("\n" ++)
      terminateCRLF l = (l ++) . ("\r\n" ++)
      blankLine = []

-- ppHeader :: (String, String) -> String
-- ppHeader (f,v) = f ++ ": " ++ v

-- NOTE: does not enforce line length limits
ppHeader :: Header String -> [String]
ppHeader (To mbs) = ["To: " ++ ppMailboxes mbs]
ppHeader (ReplyTo addrs) = ["Reply-To: " ++ ppAddresses addrs]
ppHeader (Date date) = ["Date: " ++ formatTime rfc2822TimeLocale rfc2822DateFormat date]
ppHeader (Originator orig) = ppOriginator orig
ppHeader (Subject str) = ["Subject: " ++ str]

rfc2822TimeLocale = defaultTimeLocale
rfc2822DateFormat  = "%a, %d %b %Y %H:%M:%S %z"

ppOriginator :: Originator -> [String]
ppOriginator (From mb) = ["From: "  ++ ppMailbox mb]
ppOriginator (FromList mbs senderMb) = [ "From: "  ++ ppMailboxes mbs
                                       , "Sender: " ++ ppMailbox senderMb
                                       ]
-- * message combinators

-- a minimal message
-- does not includ Message-ID
message :: Originator -> ZonedTime -> Headers String -> Body -> Message String
message originator date optionalHeaders body =
    case body of
      (RFC2822Body strs) -> RFC2822 ((originatorHeaders originator) ((dateHeader date) optionalHeaders)) strs
    where
      originatorHeaders orig = ((Originator orig) :)
      dateHeader date = ((Date date)  :)

from :: Mailbox -> Originator
from mb = From mb

to :: [Mailbox] -> Header String
to addrs = To addrs -- ("To", concat (intersperse ", " (map ppMailbox addrs)))

replyTo :: [Address] -> Header String
replyTo addrs = ReplyTo addrs -- ("To", concat (intersperse ", " (map ppMailbox addrs)))

-- AHA! we should be consistent about when we do the escaping. Is it
-- at pretty print time? If so, we need to make each optional header
-- have a type, so we know how to encode the headers.
subject :: String -> Header String
subject txt = Subject txt

-- |from list -> sender -> originator
fromList :: [Mailbox] -> Mailbox -> Originator
fromList froms sender = FromList froms sender

-- |does not validate strings
optionalHeader :: str -> str -> (Headers str -> Headers str)
optionalHeader field value = (OtherHeader (field, value) :)

test msg =
    do now <- getZonedTime
       putStr $ ppMessage LF $ msg now

testSend msg =
    do now <- getZonedTime
       putStr $ ppMessage LF $ msg now
       sendmail (msg now)

sendmail :: Message String -> IO (String, String, ExitCode)
sendmail message =
    do (inh, outh, errh, ph) <- runInteractiveProcess "/usr/sbin/sendmail" ["-t","-i"] Nothing Nothing

       outm <- newEmptyMVar
       outs <- hGetContents outh

       errm <- newEmptyMVar
       errs <- hGetContents errh

       forkIO $ hPutStr inh (ppMessage LF message) >> hClose inh
       forkIO $ evaluate (length outs) >> putMVar outm ()
       forkIO $ evaluate (length errs) >> putMVar errm ()

       readMVar outm
       readMVar errm

       ec <- waitForProcess ph
       return (outs, errs, ec)


       

{-
class MkAddress a where
    address :: a -> Address

instance To Mailbox where
    address = MailboxAddress mb
-}
{- Originator:

address		= mailbox / group
mailbox		= name-addr / addr-spec
name-addr	= [display-name] angle-addr
angle-addr	= [CFWS] "<" addr-spec ">" [CFWS] / obs-angle-addr
group		= display-name ":" [mailbox-list / CFWS] ";" 
                  [CFWS]
display-name	= phrase
mailbox-list	= (mailbox *("," mailbox)) / obs-mbox-list
address-list	= (address *("," address)) / obs-addr-list

word		= atom / quoted-string
phrase		= 1*word / obs-phrase

atext           =       ALPHA / DIGIT / ; Any character except controls,
                        "!" / "#" /     ;  SP, and specials.
                        "$" / "%" /     ;  Used for atoms
                        "&" / "'" /
                        "*" / "+" /
                        "-" / "/" /
                        "=" / "?" /
                        "^" / "_" /
                        "`" / "{" /
                        "|" / "}" /
                        "~"

atom            =       [CFWS] 1*atext [CFWS]

dot-atom        =       [CFWS] dot-atom-text [CFWS]

dot-atom-text   =       1*atext *("." 1*atext)

-}


newtype Word = Word String

newtype Atom = Atom (Maybe String, String, Maybe String)

-- |If the word is:
-- word = atom \/ quoted-string
-- So, if the input string contains special characters, we can just
-- quoted-string encode it. Otherwise, use it as normal
word :: String -> Word
word = undefined

type Phrase = [Word]

encodePhrase :: String -> String
encodePhrase str = undefined
--    bool id encode all (\c -> (isAtext c) || (isCFWS c))


{- addr-spec

addr-spec       =       local-part "@" domain

local-part      =       dot-atom / quoted-string / obs-local-part

domain          =       dot-atom / domain-literal / obs-domain

domain-literal  =       [CFWS] "[" *([FWS] dcontent) [FWS] "]" [CFWS]

dcontent        =       dtext / quoted-pair

dtext           =       NO-WS-CTL /     ; Non white space controls

                        %d33-90 /       ; The rest of the US-ASCII
                        %d94-126        ;  characters not including "[",
                                        ;  "]", or "\"

-}

addrSpec :: String -> String -> Mailbox
addrSpec localPart domainPart =
    AddrSpec localPart domainPart 

nameAddr :: String -> String -> String -> Mailbox
nameAddr displayName localPart domainPart =
    NameAddr  displayName localPart domainPart 

-- (encodePhrase displayName)  (bool id encodeQuotedString isDotAtom localPart) (bool id encodeDomainLiteral isDotAtom domainPart)

-- * Escaping Stuff

isDotAtom :: String -> Bool
isDotAtom = all (\c -> isAtext c || (c == '.'))

isAtext c = isALPHA c || (isDigit c) || (c `elem` "!#$%&'*+-/=?^_`{}|~")

-- only includes 'a'..'z' and 'A'..'Z'
isALPHA c = (('a' <= c) && (c <= 'z')) || 
            (('A' <= c) && (c <= 'Z'))

isNoWsCtl c =
    ((1 <= o) && (o <= 8)) ||
    (o == 11) ||
    (o == 12) ||
    ((14 <= o) && (o <= 31)) ||
    (o == 127)
    where
      o = ord c

quotedPair :: Char -> ShowS
quotedPair c = ('\\' :) . (c :)

isDtext c = (isNoWsCtl c) || ((33 <= o) && (o <= 90)) || ((94 <= o) && (o <= 126))
    where
      o = ord c

-- ** quoted-string

isQtext c = (isNoWsCtl c) || ((33 <= o) && (o <= 126) && (c /= '\\') && (c /= '"'))
    where
      o = ord c

encodeQuotedString :: String -> String
encodeQuotedString [] = []
encodeQuotedString str =
    '"' : foldr encode "\"" str
    where
      encode c
          | isQtext c  = (c :)
          | otherwise  = quotedPair c

-- ** domain literal 

encodeDomainLiteral :: String -> String
encodeDomainLiteral [] = []
encodeDomainLiteral str =
    '[' : foldr encode "]" str
    where
      encode c
          | isDtext c = (c :)
          | otherwise = quotedPair c

-- * Helper Functions

bool :: (a -> b) -> (a -> b) -> (a -> Bool) -> a -> b
bool t f p a = if p a then t a else f a


{-
-- only includes 'a'..'z', 'A'..'Z', and '0'..'9'
isALPHANUM c = (('a' <= c) && (c <= 'z')) || 
               (('A' <= c) && (c <= 'Z')) ||
               (('0' <= c) && (c <= '9'))
-}
            
{-

Places we might need I/O when making an message:

 1) Date field
 2) Creating unique boundary markers
 3) Message ID

Of these, the user probably only wants to control 'Date'.

The boundary marker does not have to be random, it just has to be
unique.

A Message-ID refers to a particular version of a particular
message. It pertains to exactly one instantiation of a particular
message.

So, perhaps Message-ID is created at 'send' time. For example, we
might have a 'finalize' function that adds the last bits onto a
pre-constructed message, like the current date and a new Message-ID.

-}
{-
test =
    do now <- getCurrentTime
       message from now body
    where
      from = undefined
      body = undefined
-}

{-
message :: MailboxList -> String -> AddressList -> 
message originator subject destination
-}
{-
mime contentType body =
    MIME headers body
    where
      mimeVersion = ("MIME-Version", "1.0")
-}
--  (ContentType, ContentTransferEncoding, MIMEBody str) -> (Message str)

{-
singlePart :: (Headers str) -> str -> (MIMEBody str)
singlePart headers body
-}