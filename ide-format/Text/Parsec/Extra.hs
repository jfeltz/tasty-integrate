{-
Copyright (c) 2012 John P. Feltz <jfeltz@gmail.com>
License: BSD-Style, See LICENSE
-}

--Tweaked from parsec-extra-1.0.2
--License BSD3
--Original Author:  Arie Peterson,

module Text.Parsec.Extra
  ( eol
  , digit
  , natural
  , integer
  , whitespace
  ) where

import           Control.Applicative    (Applicative,(<$>),(<*>),(*>),pure)
import           Data.List              (foldl')
import           Text.Parsec.Prim       ((<|>),(<?>))
import           Text.Parsec.String       (GenParser)
import           Text.Parsec.Combinator (many1,option)
import qualified Text.Parsec.Char as Char
import           Text.Parsec.Char       (char)
--import           Text.Parsec.String (Parser)

-- | Parse \"end of line\": one of \"\\n\", \"\\r\\n\", or \"\\r\".
eol :: GenParser Char state ()
eol = (char '\n' <|> (char '\r' >> option '\n' (char '\n'))) >> return ()

-- | A decimal digit.
digit :: (Integral a) => GenParser Char state a
digit = fromIntegral . (\ c -> fromEnum c - fromEnum '0') <$> Char.digit

-- | A natural (i.e. non-negative integer) number, in decimal notation.
natural :: (Integral a) => GenParser Char state a
natural = (foldl' (\ a b -> a * 10 + b) 0 <$> many1 digit) <?> "nonnegative decimal integer"

-- | An integer number, in decimal notation (possibly prefixed with \"-\").
integer :: (Integral a) => GenParser Char state a
integer = (option id (char '-' *> pure negate) <*> natural) <?> "decimal integer"

-- | A substitute for the otherwise misleading Parsec space parsers
whitespace :: GenParser Char state Char
whitespace = char '\SP'
