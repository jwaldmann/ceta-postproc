{-

this is a version of CeTA that can be used as a "post-processor" on starexec,
https://wiki.uiowa.edu/display/stardev/User+Guide#UserGuide-Post-Processors

it is called with two arguments:
first: name of file containing solver's output, 
second: name of file containing the benchmark

note: contrary to what the spec says,
the first file contains stdin and stderr merged,
and prepended with timestamps.

these modifications (C) Johannes Waldmann 2014
-}

{-
Copyright 2009-2013 Christian Sternagel, René Thiemann

This file is part of IsaFoR/CeTA.

IsaFoR/CeTA is free software: you can redistribute it and/or modify it under the
terms of the GNU Lesser General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

IsaFoR/CeTA is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public License along
with IsaFoR/CeTA. If not, see <http://www.gnu.org/licenses/>.
-}

module Main(main) where 

import qualified Ceta -- the certifier

import TPDB.Input (get_trs)

import System.Environment -- for getArgs
import System.IO -- for file reading
import System.Exit -- for error codes

main = do 
    args <- getArgs
    case args of
        [ outfile, benchfile ] -> do

            bench <- TPDB.Input.get_trs benchfile
            
            stamped_out <- readFile outfile
            let claim : out = map remove_timestamp $ lines $ stamped_out
                problemString = unlines out

                (status, msg) = certify False $ problemString
            
            print status
            putStrLn msg

remove_timestamp :: String -> String
remove_timestamp = unwords . drop 1 . words

data Status = INPUT_MISMATCH
            | CLAIM_MISMATCH
            | CERTIFIED | REJECTED | UNSUPPORTED | UNKNOWN_ERROR
  deriving (Eq, Show )
            

certify a problemString = 
    case Ceta.certify_proof a problemString of
         Ceta.Sumbot (Ceta.Inr (Ceta.Certified prf)) -> 
             ( CERTIFIED, prf )
         Ceta.Sumbot (Ceta.Inr (Ceta.Error message)) -> 
             ( REJECTED, message )
         Ceta.Sumbot (Ceta.Inr (Ceta.Unsupported message)) -> 
             ( UNSUPPORTED,  message )
         Ceta.Sumbot (Ceta.Inl message) -> 
             ( UNKNOWN_ERROR,  message )



