{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module Uint where {


import qualified Prelude;
import qualified Data.Word;
import qualified Data.Int;
import qualified Data.Bits;

type Int = Data.Int.Int;

type Word = Data.Word.Word;

dflt_size :: Prelude.Integer;
dflt_size = Prelude.toInteger (bitSize_aux (0::Word))
  where {
    bitSize_aux :: (Data.Bits.Bits a, Prelude.Bounded a) => a -> Uint.Int;
    bitSize_aux = Data.Bits.bitSize
  };


}
