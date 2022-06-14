
module IArray(IArray, tabulate, of_list, sub, length) where {

  import Prelude (Bool(True, False), not, Maybe(Nothing, Just),
    Integer, (+), (-), (<), fromInteger, toInteger, map, seq, (.));
  import qualified Prelude;
  import qualified Data.Array.IArray;
  import qualified Data.Array.Base;
  import qualified Data.Ix;

  newtype IArray e = IArray (Data.Array.IArray.Array Integer e);

  tabulate :: (Integer, (Integer -> e)) -> IArray e;
  tabulate (k, f) = IArray (Data.Array.IArray.array (0, k - 1) (map (\i -> let fi = f i in fi `seq` (i, fi)) [0..k - 1]));

  of_list :: [e] -> IArray e;
  of_list l = IArray (Data.Array.IArray.listArray (0, (toInteger . Prelude.length) l - 1) l);

  sub :: (IArray e, Integer) -> e;
  sub (IArray v, i) = v `Data.Array.Base.unsafeAt` fromInteger i;

  length :: IArray e -> Integer;
  length (IArray v) = toInteger (Data.Ix.rangeSize (Data.Array.IArray.bounds v));

}
