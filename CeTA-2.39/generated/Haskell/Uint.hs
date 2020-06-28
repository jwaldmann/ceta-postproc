module Uint(Int, Word, dflt_size) where

  import qualified Prelude
  import Data.Int(Int)
  import Data.Word(Word)
  import qualified Data.Bits

  dflt_size :: Prelude.Integer
  dflt_size = Prelude.toInteger (bitSize_aux (0::Word)) where
    bitSize_aux :: (Data.Bits.Bits a, Prelude.Bounded a) => a -> Int
    bitSize_aux = Data.Bits.bitSize
