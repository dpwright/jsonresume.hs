{-# LANGUAGE OverloadedStrings #-}

-- | This module provides a native Haskell representation for the JSON Resume
-- scheme, as defined at
-- <https://github.com/jsonresume/resume-schema/blob/master/schema.json>, as well
-- as instances for Aeson's FromJSON/ToJSON classes to make parsing the files
-- easy.
--
-- Note that nearly all the fields are wrapped in a Maybe type.  This is because
-- the JSON scheme on which this is based doesn't specify one way or the other
-- whether a field is required, and in JSON essentially all the fields are
-- optional.

module Data.JSONResume
  ( Resume (..)
  , URL
  , EmailAddress
  , Address (..)
  , Profile (..)
  , Basics (..)
  , Organization (..)
  , Work (..)
  , Volunteer (..)
  , Education (..)
  , Award (..)
  , Publication (..)
  , Skill (..)
  , Language (..)
  , Interest (..)
  , Reference (..)
  ) where

import qualified Data.Text           as T
import qualified Data.HashMap.Strict as H

import System.Locale       (defaultTimeLocale)
import Data.Time           (UTCTime)
import Data.Time.Format    (formatTime, parseTime)
import Data.Aeson          (ToJSON (..), FromJSON (..), Value (..),
                            object, (.:?), (.!=), (.=), withText)
import Data.Aeson.Types    (Parser)

import Control.Applicative (Applicative (..), (<$>), (<*>), pure)
import Control.Monad       (mzero)

-- | This is the main datatype, representing the overall structure of the JSON
-- Resume specification.
data Resume = Resume
            { basics       :: Maybe Basics
            , work         :: [Work]
            , volunteer    :: [Volunteer]
            , education    :: [Education]
            , awards       :: [Award]
            , publications :: [Publication]
            , skills       :: [Skill]
            , languages    :: [Language]
            , interests    :: [Interest]
            , references   :: [Reference]
            } deriving (Read, Show)

instance FromJSON Resume where
  parseJSON (Object v) =
    Resume <$> v .:? "basics"
           <*> v .:? "work"         .!= []
           <*> v .:? "volunteer"    .!= []
           <*> v .:? "education"    .!= []
           <*> v .:? "awards"       .!= []
           <*> v .:? "publications" .!= []
           <*> v .:? "skills"       .!= []
           <*> v .:? "languages"    .!= []
           <*> v .:? "interests"    .!= []
           <*> v .:? "references"   .!= []
  parseJSON _ = mzero

instance ToJSON Resume where
  toJSON (Resume b w v e a p s l i r) = object
    [ "basics"       .= b
    , "work"         .= w
    , "volunteer"    .= v
    , "education"    .= e
    , "awards"       .= a
    , "publications" .= p
    , "skills"       .= s
    , "languages"    .= l
    , "interests"    .= i
    , "references"   .= r
    ]

-- | Simple representation for URLs
type URL = T.Text

-- | Simple representation for email addresses
type EmailAddress = T.Text

-- | Represents a physical address
data Address = Address
             { address     :: Maybe T.Text -- ^ To add multiple address lines, use \n. For example, 1234 Glücklichkeit Straße\nHinterhaus 5. Etage li.
             , postalCode  :: Maybe T.Text
             , city        :: Maybe T.Text
             , countryCode :: Maybe T.Text -- ^ Code as per ISO-3166-1 ALPHA-2, e.g. US, AU, IN
             , region      :: Maybe T.Text -- ^ The general region where you live. Can be a US state, or a province, for instance.
             } deriving (Read, Show)

instance FromJSON Address where
  parseJSON (Object v) =
    Address <$> v .:? "address"
            <*> v .:? "postalCode"
            <*> v .:? "city"
            <*> v .:? "countryCode"
            <*> v .:? "region"
  parseJSON _ = mzero

instance ToJSON Address where
  toJSON (Address a p c cc r) = object
    [ "address"     .= a
    , "postalCode"  .= p
    , "city"        .= c
    , "countryCode" .= cc
    , "region"      .= r
    ]

-- | Specify any number of social networks that you participate in
data Profile = Profile
             { network  :: Maybe T.Text -- ^ e.g. Facebook or Twitter
             , username :: Maybe T.Text -- ^ e.g. neutralthoughts
             , url      :: Maybe URL    -- ^ e.g. http://twitter.com/neutralthoughts
             } deriving (Read, Show)

instance FromJSON Profile where
  parseJSON (Object v) =
    Profile <$> v .:? "network"
            <*> v .:? "username"
            <*> v .:? "url"
  parseJSON _ = mzero

instance ToJSON Profile where
  toJSON (Profile n u web) = object
    [ "network"  .= n
    , "username" .= u
    , "url"      .= web
    ]

-- | Basic information
data Basics = Basics
            { name     :: Maybe T.Text
            , label    :: Maybe T.Text       -- ^ e.g. Web Developer
            , picture  :: Maybe URL          -- ^ URL (as per RFC 3986) to a picture in JPEG or PNG format
            , email    :: Maybe EmailAddress -- ^ e.g. thomas@gmail.com
            , phone    :: Maybe T.Text       -- ^ Phone numbers are stored as strings so use any format you like, e.g. 712-117-2923
            , website  :: Maybe URL          -- ^ URL (as per RFC 3986) to your website, e.g. personal homepage
            , summary  :: Maybe T.Text       -- ^ Write a short 2-3 sentence biography about yourself
            , location :: Maybe Address
            , profiles :: [Profile]
            } deriving (Read, Show)

instance FromJSON Basics where
  parseJSON (Object v) =
    Basics <$> v .:? "name"
           <*> v .:? "label"
           <*> v .:? "picture"
           <*> v .:? "email"
           <*> v .:? "phone"
           <*> v .:? "website"
           <*> v .:? "summary"
           <*> v .:? "location"
           <*> v .:? "profiles" .!= []
  parseJSON _ = mzero

instance ToJSON Basics where
  toJSON (Basics n l pic e phn w s loc ps) = object
    [ "name"     .= n
    , "label"    .= l
    , "picture"  .= pic
    , "email"    .= e
    , "phone"    .= phn
    , "website"  .= w
    , "summary"  .= s
    , "location" .= loc
    , "profiles" .= ps
    ]

-- | Information about a particular organization that you've worked or
-- volunteered at
data Organization = Organization
                  { orgName          :: Maybe T.Text   -- ^ e.g. Facebook
                  , orgPosition      :: Maybe T.Text   -- ^ e.g. Software Engineer
                  , orgSite          :: Maybe URL      -- ^ e.g. http://facebook.com
                  , orgStartDate     :: Maybe UTCTime  -- ^ resume.json uses the ISO 8601 date standard e.g. 2014-06-29
                  , orgEndDate       :: Maybe UTCTime  -- ^ e.g. 2012-06-29
                  , orgSummary       :: Maybe T.Text   -- ^ Give an overview of your responsibilities at the company
                  , orgHighlights    :: [T.Text] -- ^ Specify multiple accomplishments, e.g. Increased profits by 20% from 2011-2012 through viral advertising
                  } deriving (Read, Show)

-- | Specify that you worked at a particular @Organization@ (as opposed to
-- volunteering there)
newtype Work = Work Organization deriving (Read, Show)

instance FromJSON Work where
  parseJSON (Object v) = fmap Work $
    Organization <$> v .:? "company"
                 <*> v .:? "position"
                 <*> v .:? "website"
                 <*> potentially dateFromJSON v "startDate"
                 <*> potentially dateFromJSON v "endDate"
                 <*> v .:? "summary"
                 <*> v .:? "highlights" .!= []
  parseJSON _ = mzero

instance ToJSON Work where
  toJSON (Work (Organization n p web start end smry hl)) = object
    [ "company"    .= n
    , "position"   .= p
    , "website"    .= web
    , ("startDate",   toJSON $ dateToJSON <$> start)
    , ("endDate",     toJSON $ dateToJSON <$> end)
    , "summary"    .= smry
    , "highlights" .= hl
    ]

-- | Specify that you volunteered at a particular @Organization@ (as opposed to
-- working there)
newtype Volunteer = Volunteer Organization deriving (Read, Show)

instance FromJSON Volunteer where
  parseJSON (Object v) = fmap Volunteer $
    Organization <$> v .:? "organization"
                 <*> v .:? "position"
                 <*> v .:? "website"
                 <*> potentially dateFromJSON v "startDate"
                 <*> potentially dateFromJSON v "endDate"
                 <*> v .:? "summary"
                 <*> v .:? "highlights" .!= []
  parseJSON _ = mzero

instance ToJSON Volunteer where
  toJSON (Volunteer (Organization n p web start end smry hl)) = object
    [ "organization" .= n
    , "position"     .= p
    , "website"      .= web
    , ("startDate",     toJSON $ dateToJSON <$> start)
    , ("endDate",       toJSON $ dateToJSON <$> end)
    , "summary"      .= smry
    , "highlights"   .= hl
    ]

-- | Educational history
data Education = Education
               { institution :: Maybe T.Text   -- ^ e.g. Massachusetts Institute of Technology
               , area        :: Maybe T.Text   -- ^ e.g. Arts
               , studyType   :: Maybe T.Text   -- ^ e.g. Bachelor
               , startDate   :: Maybe UTCTime  -- ^ e.g. 2012-06-29
               , endDate     :: Maybe UTCTime  -- ^ e.g. 2014-06-29
               , gpa         :: Maybe T.Text   -- ^ grade point average, e.g. 3.67/4.0
               , courses     :: [T.Text] -- ^ List notable courses/subjects, e.g. H1302 - Introduction to American history
               } deriving (Read, Show)

instance FromJSON Education where
  parseJSON (Object v) =
    Education <$> v .:? "institution"
              <*> v .:? "area"
              <*> v .:? "studyType"
              <*> potentially dateFromJSON v "startDate"
              <*> potentially dateFromJSON v "endDate"
              <*> v .:? "gpa"
              <*> v .:? "courses" .!= []
  parseJSON _ = mzero

instance ToJSON Education where
  toJSON (Education i a t start end g cs) = object
    [ "institution" .= i
    , "area"        .= a
    , "studyType"   .= t
    , ("startDate",    toJSON $ dateToJSON <$> start)
    , ("endDate",      toJSON $ dateToJSON <$> end)
    , "gpa"         .= g
    , "courses"     .= cs
    ]

-- | Specify any awards you have received throughout your professional career
data Award = Award
           { title        :: Maybe T.Text  -- ^ e.g. One of the 100 greatest minds of the century
           , date         :: Maybe UTCTime -- ^ e.g. 1989-06-12
           , awarder      :: Maybe T.Text  -- ^ e.g. Time Magazine
           , awardSummary :: Maybe T.Text  -- ^ e.g. Received for my work with Quantum Physics
           } deriving (Read, Show)

instance FromJSON Award where
  parseJSON (Object v) =
    Award <$> v .:? "title"
          <*> potentially dateFromJSON v "date"
          <*> v .:? "awarder"
          <*> v .:? "summary"
  parseJSON _ = mzero

instance ToJSON Award where
  toJSON (Award t d a s) = object
    [ "title"   .= t
    , ("date",     toJSON $ dateToJSON <$> d)
    , "awarder" .= a
    , "summary" .= s
    ]

-- | Specify your publications through your career
data Publication = Publication
                 { pubName        :: Maybe T.Text  -- ^ e.g. The World Wide Web
                 , publisher      :: Maybe T.Text  -- ^ e.g. IEEE, Computer Magazine
                 , pubReleaseDate :: Maybe UTCTime -- ^ e.g. 1990-08-01
                 , pubSite        :: Maybe URL     -- ^ e.g. http://www.computer.org/csdl/mags/co/1996/10/rx069-abs.html
                 , pubSummary     :: Maybe T.Text  -- ^ Short summary of publication. e.g. Discussion of the World Wide Web, HTTP, HTML.
                 } deriving (Read, Show)

instance FromJSON Publication where
  parseJSON (Object v) =
    Publication <$> v .:? "name"
                <*> v .:? "publisher"
                <*> potentially dateFromJSON v "releaseDate"
                <*> v .:? "website"
                <*> v .:? "summary"
  parseJSON _ = mzero

instance ToJSON Publication where
  toJSON (Publication n p d web s) = object
    [ "name"        .= n
    , "publisher"   .= p
    , ("releaseDate",  toJSON $ dateToJSON <$> d)
    , "website"     .= web
    , "summary"     .= s
    ]

-- | List out your professional skill-set
data Skill = Skill
           { skillName     :: Maybe T.Text   -- ^ e.g. Web Development
           , skillLevel    :: Maybe T.Text   -- ^ e.g. Master
           , skillKeywords :: [T.Text] -- ^ List some keywords pertaining to this skill, e.g. HTML
           } deriving (Read, Show)

instance FromJSON Skill where
  parseJSON (Object v) =
    Skill <$> v .:? "name"
          <*> v .:? "level"
          <*> v .:? "keywords" .!= []
  parseJSON _ = mzero

instance ToJSON Skill where
  toJSON (Skill n l ks) = object
    [ "name"     .= n
    , "level"    .= l
    , "keywords" .= ks
    ]

-- | List any other languages you speak
data Language = Language
              { language :: Maybe T.Text -- ^ e.g. English, Spanish
              , fluency  :: Maybe T.Text -- ^ e.g. Fluent, Beginner
              } deriving (Read, Show)

instance FromJSON Language where
  parseJSON (Object v) =
    Language <$> v .:? "language"
             <*> v .:? "fluency"
  parseJSON _ = mzero

instance ToJSON Language where
  toJSON (Language l f) = object
    [ "language" .= l
    , "fluency"  .= f
    ]

data Interest = Interest
              { interestName     :: Maybe T.Text   -- ^ e.g. Philosophy
              , interestKeywords :: [T.Text] -- ^ e.g. Friedrich Nietzsche
              } deriving (Read, Show)

instance FromJSON Interest where
  parseJSON (Object v) =
    Interest <$> v .:? "name"
             <*> v .:? "keywords" .!= []
  parseJSON _ = mzero

instance ToJSON Interest where
  toJSON (Interest n ks) = object
    [ "name"     .= n
    , "keywords" .= ks
    ]

-- | List any references you have received
data Reference = Reference
               { refName   :: Maybe T.Text -- ^ e.g. Timothy Cook
               , reference :: Maybe T.Text -- ^ e.g. Joe blogs was a great employee, who turned up to work at least once a week. He exceeded my expectations when it came to doing nothing.
               } deriving (Read, Show)

instance FromJSON Reference where
  parseJSON (Object v) =
    Reference <$> v .:? "name"
              <*> v .:? "reference"
  parseJSON _ = mzero

instance ToJSON Reference where
  toJSON (Reference n r) = object
    [ "name"      .= n
    , "reference" .= r
    ]

-- A couple of utility functions to help with parsing --

-- Write out a date using JSON Resume's preferred date format (YYYY-mm-dd)
dateToJSON :: UTCTime -> Value
dateToJSON t = String $ T.pack $ formatTime defaultTimeLocale "%F" t

-- Read in a date using JSON Resume's preferred date format (YYYY-mm-dd)
dateFromJSON :: Value -> Parser UTCTime
dateFromJSON = withText "UTCTime" $ \t ->
  case parseTime defaultTimeLocale "%F" (T.unpack t) of
    Just d -> pure d
    _      -> fail "could not parse ISO-8601 date"

-- A version of (.:?) which is parameterised on the parsing function to use
potentially :: (Value -> Parser a) -> H.HashMap T.Text Value -> T.Text -> Parser (Maybe a)
potentially f obj key = case H.lookup key obj of
  Nothing -> pure Nothing
  Just v  -> Just <$> f v
