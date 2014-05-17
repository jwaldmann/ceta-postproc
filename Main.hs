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

original copyright notice follows:
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

{-# language OverloadedStrings #-}

module Main(main) where 

import qualified Ceta -- the certifier

import TPDB.Input (get_trs)
import TPDB.Pretty
import TPDB.CPF.Proof.Type as CPF
import TPDB.CPF.Proof.Read as CPF

import System.Environment -- for getArgs
import System.IO -- for file reading
import System.Exit -- for error codes
import Control.Monad ( when )

main = do 
    args <- getArgs
    case args of
        [ outfile, benchfile ] -> handle True outfile benchfile
        [ "-n", outfile, benchfile ] -> handle False outfile benchfile

handle remove_stamps outfile benchfile = do
    bench <- TPDB.Input.get_trs benchfile
    
    out <- readFile outfile
    let process = if remove_stamps then remove_timestamp else id
        claim_string : proof = map process $ lines out
        claim = case claim_string of
              "YES" -> Just True
              "NO"  -> Just False
              _     -> Nothing
        problemString = unlines proof

        (cert, msg) = certify False $ problemString

    [ p ] <- CPF.readCP problemString

    -- claim conforms to proof
    when ( bench /= CPF.trsinput_trs ( CPF.input p ) )
         $ whine INPUT_MISMATCH 
         $ vcat [ "benchmark:" <+> pretty bench
                , "proof.input:" <+> pretty ( CPF.trsinput_trs $ CPF.input p ) ]

    -- input from benchfile conforms to input from proof
    case (claim, CPF.proof p) of
         (Just True , CPF.TrsTerminationProof {} ) -> return ()
         (Just False, CPF.TrsNonterminationProof {} ) -> return ()
         _ -> whine CLAIM_MISMATCH $ pretty claim
    
    whine cert $ text msg     

remove_timestamp :: String -> String
remove_timestamp = unwords . drop 1 . words

whine :: Status -> Doc -> IO ()
whine status doc = do
    putStrLn $ show status
    case status of
        CERTIFIED -> print doc
        _ -> error $ show doc

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



