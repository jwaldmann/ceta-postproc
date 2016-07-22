{-# LANGUAGE EmptyDataDecls, RankNTypes, ScopedTypeVariables #-}

module IArray where {


  import qualified Data.Array.IArray;
  import qualified Data.Array.Base;
  import qualified Data.Ix;
  import qualified System.IO;
  import qualified Data.List;

  -- The following is largely inspired by the heap monad theory in the Imperative HOL Library

  -- We restrict ourselves to immutable arrays whose indexes are Integer

  type IArray e = Data.Array.IArray.Array Integer e;

  -- The following function constructs an immutable array from an upper bound and a function;
  -- It is the equivalent to SML Vector.of_fun:

  array :: (Integer -> e) -> Integer -> IArray e;
  array f k = Data.Array.IArray.array (0, k - 1) (map (\i -> let fi = f i in fi `seq` (i, f i)) [0..k - 1]) ;
  
  -- The following function is the equivalent to "IArray" type constructor in the SML code
  -- generation setup;
  -- The function length returns a term of type Int, from which we cast to an Integer
  
  listIArray :: [e] -> IArray e;
  listIArray l = Data.Array.IArray.listArray (0, (toInteger . length) l - 1) l;

  -- The access operation for IArray, denoted by ! as an infix operator;
  -- in SML it was denoted as "Vector.sub";
  -- note that SML "Vector.sub" has a single parameter, a pair, 
  -- whereas Haskell "(!)"  has two different parameters; 
  -- that's why we introduce "sub" in Haskell

  infixl 9 !;

  (!) :: IArray e -> Integer -> e;
  v ! i = v `Data.Array.Base.unsafeAt` fromInteger i;

  sub :: (IArray e, Integer) -> e;
  sub (v, i) = v ! i;
  
  -- We use the name lengthIArray to avoid clashes with Prelude.length, usual length for lists:

  lengthIArray :: IArray e -> Integer;
  lengthIArray v = toInteger (Data.Ix.rangeSize (Data.Array.IArray.bounds v));

  -- An equivalent to the Vector.find SML function;
  -- we introduce an auxiliary recursive function
  
  findr :: (e -> Bool) -> Integer -> IArray e -> Maybe e;
  findr f i v = (if ((lengthIArray v - 1) < i) then Nothing
                               else case f (v ! i) of 
                                         True -> Just (v ! i)
                                         False -> findr f (i + 1) v);

  -- The definition of find is as follows

  find :: (e -> Bool) -> IArray e -> Maybe e;
  find f v = findr f 0 v;
  
  -- The definition of the SML function "Vector.exists", based on "find"

  existsIArray :: (e -> Bool) -> IArray e -> Bool;
  existsIArray f v = (case (find f v) of {Nothing -> False;
                                           _      -> True});

  -- The definition of the SML function "Vector.all", based on Haskell in "existsIArray"

  allIArray :: (e -> Bool) -> IArray e -> Bool;
  allIArray f v = not (existsIArray (\x -> not (f x)) v);  


}
