module Text.MIME.Type where

import Data.Time.LocalTime (ZonedTime(..))
import Data.Time.Format ()

data ContentType 
    = ContentType (String, String) [Parameter]
      deriving (Show, Read, Eq)

data ContentTransferEncoding
    = SevenBit
    | EightBit
    | Binary
    | QuotedPrintable
    | Base64
      deriving (Show, Read, Eq)

type Parameter = (String, String)

-- | A Message
data Message str
    = MIME (Headers str) (MIMEBody str)
    | RFC2822 (Headers str) [str]
      deriving (Show, Read)

data MIMEBody str
    = Multipart MultipartType [Message str]
    | Singlepart str
    | Message (Message str)
      deriving (Show, Read)

data MultipartType
    = Alternative
    | Digest
    | Parallel
    | Mixed
    | OtherMultipart
      deriving (Show, Read)

type Headers str = [Header str]

-- ZonedTime has no Read instance, so..
data Header str 
    = To [Mailbox]
    | ReplyTo [Address]
    | Date ZonedTime
    | Originator Originator
    | Subject str
    | OtherHeader (str, str)
      deriving (Show, Read)

data Originator
    = From Mailbox
    | FromList [Mailbox] Mailbox
      deriving (Read, Show)

data Date = Rfc2822Date
          deriving (Read, Show)

data Body 
    = RFC2822Body [String]
      deriving (Read, Show)

data Address
    = MailboxAddress Mailbox
    | GroupAddress Group
      deriving (Read, Show)

type DisplayName = String
data Group = Group DisplayName [Mailbox] deriving (Read, Show)

type LocalPart = String
type DomainPart = String
data Mailbox
    = AddrSpec LocalPart DomainPart
    | NameAddr DisplayName LocalPart DomainPart
      deriving (Show, Read)
