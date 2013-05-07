{-# OPTIONS_GHC -XDeriveDataTypeable #-} 
module SudokuExceptions where

import Control.Exception
import Data.Typeable

data InvalidSudokuFormatException = InvalidSudokuFormatException
  deriving (Show,Typeable)

instance Exception InvalidSudokuFormatException

data UnsolvableSudokuException = UnsolvableSudokuException
  deriving (Show,Typeable)

instance Exception UnsolvableSudokuException

data HeadException = HeadException 
  deriving (Show,Typeable)

instance Exception HeadException

data TailException = TailException
  deriving (Show,Typeable)

instance Exception TailException
