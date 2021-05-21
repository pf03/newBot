{-# LANGUAGE FlexibleInstances #-}

module Interface.MError.Types where

import qualified Control.Exception as E

data E
  = ParseError String
  | QueryError String
  | ConfigError String
  | IOError String
  | -- | An error that should never occur when the program is written correctly
    -- (for example, incorrect pattern matching)
    DevError String
  | SomeError String
  | Exit

type EE = Either E

instance Show E where
  show (ParseError s) = "Parse JSON error: " ++ s
  show (QueryError s) = "Query error: " ++ s
  show (ConfigError s) = "Config error: " ++ s
  show (IOError s) = "IO error: " ++ s
  show (DevError s) = "Developer error: " ++ s
  show (SomeError s) = "Some error: " ++ s
  show Exit = "Exit from application by user choice"

instance E.Exception E

instance MonadFail (Either E) where
  fail s = Left $ DevError s