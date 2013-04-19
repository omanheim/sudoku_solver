{-# OPTIONS_GHC -XDeriveDataTypeable #-} 
module SudokuExceptions where

import Control.Exception
import Data.Typeable

data InvalidSudokuFormatException = InvalidSudokuFormatException
  deriving (Show,Typeable)

instance Exception InvalidSudokuFormatException
