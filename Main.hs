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

data Starexec_Result = CERTIFIED | REJECTED | IGNORED
     deriving (Eq, Show)
data Original_Result = YES | NO | MAYBE 
     deriving (Eq, Show)
data Consistency = CONSISTENT | INPUT_MISMATCH | CLAIM_MISMATCH | PARSE_ERROR
     deriving (Eq, Show)


handle on_star_exec outfile benchfile = do
    bench <- TPDB.Input.get_trs benchfile
    
    out <- readFile outfile
    let process = if on_star_exec then remove_timestamp else id
        claim_string : proof = map process $ lines out
        claim = case claim_string of
              "YES" -> YES
              "NO"  -> NO
              _     -> MAYBE
        problemString = unlines 
                      $ takeWhile ( /= "EOF" ) -- FIXME (issue #1)
                      $ proof

        (cert, msg) = certify False $ problemString

    when (claim == MAYBE) $ whine on_star_exec [("starexec-result", show claim)] empty

    ps <- CPF.readCP problemString
    when (length ps /= 1) 
        $ whine on_star_exec [("starexec-result", "postproc-parse-error")
                             ,("original-result", show claim) ] empty

    let [ p ] = ps

    when ( bench /= CPF.trsinput_trs ( CPF.input p ) )
         $ whine on_star_exec [("starexec-result", "REJECTED")
                              ,("original-result", show claim)
                              ,("consistency", "INPUT_MISMATCH")
                              ]
         $ vcat [ "benchmark:" <+> pretty bench
                , "proof.input:" <+> pretty ( CPF.trsinput_trs $ CPF.input p ) ]

    case (claim, CPF.proof p) of
         (YES , CPF.TrsTerminationProof {} ) -> return ()
         (NO, CPF.TrsNonterminationProof {} ) -> return ()
         _ -> whine on_star_exec [("starexec-result", "REJECTED")
                                 ,("original-result", show claim)
                                 ,("consistency", "CLAIM_MISMATCH")] empty

    case cert of
        Left reason -> whine on_star_exec [("starexec-result", "REJECTED")
                                          ,("original-result", show claim)
                                          ,("consistency", "CONSISTENT")
                                          ,("certification-result", reason)] $ text msg
        Right reason -> whine on_star_exec [("starexec-result", "CERTIFIED")
                                          ,("original-result", show claim)
                                          ,("consistency", "CONSISTENT")
                                          ,("certification-result", reason)] $ text msg


remove_timestamp :: String -> String
remove_timestamp = unwords . drop 1 . words

whine :: Bool -> [(String,String)] -> Doc -> IO ()
whine on_star_exec keyvals doc = do
    putStrLn $ unlines $ map (\(k,v) -> k ++ "=" ++ v) keyvals
    error $ if on_star_exec then "" else show doc

certify a problemString = 
    case Ceta.certify_proof a problemString of
         Ceta.Sumbot (Ceta.Inr (Ceta.Certified prf)) -> 
             ( Right "CERTIFIED", prf )
         Ceta.Sumbot (Ceta.Inr (Ceta.Error message)) -> 
             ( Left "REJECTED", message )
         Ceta.Sumbot (Ceta.Inr (Ceta.Unsupported message)) -> 
             ( Left "UNSUPPORTED",  message )
         Ceta.Sumbot (Ceta.Inl message) -> 
             ( Left "UNKNOWN_ERROR",  message )



