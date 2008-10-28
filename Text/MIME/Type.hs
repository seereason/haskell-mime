{-# LANGUAGE DeriveDataTypeable #-}
module Text.MIME.Type where

import Data.Time.LocalTime (ZonedTime(..))
import Data.Time.Format ()
import Data.Generics

data ContentType 
    = ContentType (String, String) [Parameter]
      deriving (Show, Read, Eq, Ord, Data, Typeable)

data ContentTransferEncoding
    = SevenBit
    | EightBit
    | Binary
    | QuotedPrintable
    | Base64
      deriving (Show, Read, Eq, Ord, Data, Typeable)

type Parameter = (String, String)

-- | A Message
data Message str
    = MIME (Headers str) (MIMEBody str)
    | RFC2822 (Headers str) [str]
      deriving Show


data MIMEBody str
    = Multipart MultipartType [Message str]
    | Singlepart str
    | Message (Message str)
      deriving Show

data MultipartType
    = Alternative
    | Digest
    | Parallel
    | Mixed
    | OtherMultipart
      deriving (Show, Read, Eq, Ord, Data, Typeable)

type Headers str = [Header str]

-- ZonedTime has no Read instance, so..
data Header str 
    = To [Address]
    | Cc [Address]
    | Bcc [Address]
    | ReplyTo [Address]
    | Date ZonedTime
    | Originator Originator
    | Subject str
    | OtherHeader (str, str)
      deriving (Show, Read)

data Originator
    = From Mailbox
    | FromList [Mailbox] Mailbox
      deriving (Show, Read, Eq, Ord, Data, Typeable)

data Date = Rfc2822Date
      deriving (Show, Read, Eq, Ord, Data, Typeable)

data Body 
    = RFC2822Body [String]
      deriving (Show, Read, Eq, Ord, Data, Typeable)

data Address
    = MailboxAddress Mailbox
    | GroupAddress Group
      deriving (Show, Read, Eq, Ord, Data, Typeable)

type DisplayName = String
data Group = Group DisplayName [Mailbox] 
      deriving (Show, Read, Eq, Ord, Data, Typeable)

type LocalPart = String
type DomainPart = String
data Mailbox
    = AddrSpecMBX AddrSpec
    | NameAddr DisplayName AddrSpec
      deriving (Show, Read, Eq, Ord, Data, Typeable)

data AddrSpec 
    = AddrSpec LocalPart DomainPart
      deriving (Show, Read, Eq, Ord, Data, Typeable)