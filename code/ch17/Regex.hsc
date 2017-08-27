-- file: ch17/Regex.hsc
{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Regex where

import Foreign
import Foreign.C.Types
import Data.ByteString (ByteString)

#include <pcre.h>

-- | A type for PCRE compile-time options. These are newtyped CInts,
-- which can be bitwise-or'd together, using '(Data.Bits..|.)'
--
newtype PCREOption = PCREOption { unPCREOption :: CInt }
    deriving (Eq,Show)


-- PCRE compile options
#{enum PCREOption, PCREOption
  , caseless             = PCRE_CASELESS
  , dollar_endonly       = PCRE_DOLLAR_ENDONLY
  , dotall               = PCRE_DOTALL
  , dupnames             = PCRE_DUPNAMES
  , extended             = PCRE_EXTENDED
  , extra                = PCRE_EXTRA
  , firstline            = PCRE_FIRSTLINE
  , multiline            = PCRE_MULTILINE
  , newline_cr           = PCRE_NEWLINE_CR
  , newline_crlf         = PCRE_NEWLINE_CRLF
  , newline_lf           = PCRE_NEWLINE_LF
  , no_auto_capture      = PCRE_NO_AUTO_CAPTURE
  , ungreedy             = PCRE_UNGREEDY
  }

-- | Combine a list of options into a single option, using bitwise (.|.)
combineOptions :: [PCREOption] -> PCREOption
combineOptions = PCREOption . foldr ((.|.) . unPCREOption) 0

data PCRE

data Regex = Regex !(ForeignPtr PCRE)
                   !ByteString
        deriving (Eq, Ord, Show)

newtype PCREInfo = PCREInfo { unPCREInfo :: CInt }
    deriving (Eq,Show)

#{enum PCREInfo, PCREInfo 
  , info_options             = PCRE_INFO_OPTIONS
  , info_size                = PCRE_INFO_SIZE
  , info_capturecount        = PCRE_INFO_CAPTURECOUNT
  , info_backrefmax          = PCRE_INFO_BACKREFMAX
  , info_firstbyte           = PCRE_INFO_FIRSTBYTE
  , info_firstchar           = PCRE_INFO_FIRSTCHAR
  , info_firsttable          = PCRE_INFO_FIRSTTABLE
  , info_lastliteral         = PCRE_INFO_LASTLITERAL
  , info_nameentrysize       = PCRE_INFO_NAMEENTRYSIZE
  , info_namecount           = PCRE_INFO_NAMECOUNT
  , info_nametable           = PCRE_INFO_NAMETABLE
  , info_studysize           = PCRE_INFO_STUDYSIZE
  , info_default_tables      = PCRE_INFO_DEFAULT_TABLES
  , info_okpartial           = PCRE_INFO_OKPARTIAL
  , info_jchanged            = PCRE_INFO_JCHANGED
  , info_hascrorlf           = PCRE_INFO_HASCRORLF
  , info_minlength           = PCRE_INFO_MINLENGTH
  }

newtype PCREExecOption = PCREExecOption { unPCREExecOption :: CInt }
    deriving (Eq,Show)

combineExecOptions :: [PCREExecOption] -> PCREExecOption
combineExecOptions = PCREExecOption . foldr ((.|.) . unPCREExecOption) 0

