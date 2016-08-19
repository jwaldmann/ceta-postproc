{-

this is a version of CeTA that can be used as a "post-processor" on starexec,
https://wiki.uiowa.edu/display/stardev/User+Guide#UserGuide-Post-Processors

it is called with two arguments:
first: name of file containing solver's output, 
second: name of file containing the benchmark

note: contrary to what the spec says,
the first file contains stdin and stderr merged,
and prepended with timestamps.

these modifications (C) Johannes Waldmann 2014, 2015, 2016

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

import qualified Complexity as C

import qualified TPDB.Data as D
import TPDB.XTC.Read ( readProblems )
import TPDB.Pretty
import TPDB.Plain.Write ()
import qualified TPDB.CPF.Proof.Type as CPF
import qualified TPDB.CPF.Proof.Read as CPF

import System.Environment -- for getArgs
import System.IO -- for file reading
import System.Exit -- for error codes
import Control.Monad ( when )
import qualified Data.Set as S

main = do 
    args <- getArgs
    case args of
        [ "-n", outfile, benchfile ] -> handle False outfile benchfile
        [ outfile, benchfile, extradir ] -> handle True outfile benchfile
        _ -> error $ unlines 
            [ "usage: ceta-postproc [-n] proof benchmark"
            , "   -n        : not on starexec (do not remove timestamps)"
            , "   proof     : pathname for CPF document (with ASCII status in first line)"
            , "   benchmark : pathname for input problem (XML TPDB format)"
            ]

data Original_Result = YES | NO | MAYBE | Bounds C.Bounds
     deriving (Eq)

instance Show Original_Result where
    show r = case r of
        YES -> "YES" ; NO -> "NO" ; MAYBE -> "MAYBE"
        Bounds b -> show b

-- data Starexec_Result = CERTIFIED | REJECTED | IGNORED
--     deriving (Eq, Show)

-- data Consistency = CONSISTENT | INPUT_MISMATCH | CLAIM_MISMATCH | PARSE_ERROR
--     deriving (Eq, Show)


handle on_star_exec outfile benchfile = do
    benches <- TPDB.XTC.Read.readProblems benchfile
    when (length benches /= 1)
      $ whine on_star_exec
      [ ("starexec-result","REJECTED")
      , ("consistency","XTC_PARSE_ERROR")
      ] empty
    let [bench] = benches
    
    out <- readFile outfile
    let process = if on_star_exec then remove_timestamp else id
        (claim, proof) = case map process $ lines out of
            [] -> (MAYBE, [])
            "YES" : proof -> (YES, proof)
            "NO" : proof -> (NO, proof)
            "MAYBE" : _ -> (MAYBE, [])
            claim_string : proof -> case readsPrec 0 claim_string of
                  [(b,"")] -> (Bounds b, proof)
                  _     -> (MAYBE, [])
        problemString = unlines 
                      $ takeWhile ( /= "EOF" ) -- FIXME (issue #1)
                      $ proof

        (cert, msg) = certify False $ problemString

    when (claim == MAYBE) $ whine on_star_exec [("starexec-result", show claim)] empty

    let accepted claim = show claim
        rejected claim = "REJECTED-" ++ show claim

    ps <- CPF.readCP problemString
    when (length ps /= 1) 
        $ whine on_star_exec [("starexec-result", rejected claim)
                             ,("original-result", show claim) 
                             ,("consistency", "CPF_PARSE_ERROR")
                             ] empty

    let [ p ] = ps

    case consistent bench $ CPF.input p  of
      Left msg -> whine on_star_exec [("starexec-result", rejected claim)
                              ,("original-result", show claim)
                              ,("consistency", "INPUT_MISMATCH")
                              ]
         $ vcat [ msg
                , "benchmark:" <+> pretty bench
                , "proof.input:" <+> pretty ( CPF.input p )
                ]
      Right () -> return ()

    when (not $ matches claim p) 
        $ whine on_star_exec [("starexec-result", rejected claim)
                                 ,("original-result", show claim)
                                 ,("consistency", "CLAIM_MISMATCH")]
        $ vcat [ text $ show claim
               -- , pretty p
               ]

    case cert of
        Left reason -> whine on_star_exec [("starexec-result", rejected claim)
                                          ,("original-result", show claim)
                                          ,("consistency", "CONSISTENT")
                                          ,("certification-result", reason)] $ text msg
        Right reason -> whine on_star_exec [("starexec-result", accepted claim)
                                          ,("original-result", show claim)
                                          ,("consistency", "CONSISTENT")
                                          ,("certification-result", reason)] $ text msg

consistent :: D.Problem CPF.Identifier CPF.Identifier
           -> CPF.CertificationProblemInput
           -> Either Doc ()
consistent p i = 
  if D.trs p /= CPF.trsinput_trs i
  then Left "trs are not equal"
  else case i of
    CPF.TrsInput {} -> do
      return () -- FIXME: absence of strategy, theory, startterm
    CPF.ComplexityInput {} -> do
      -- FIXME: absence of strategy, theory, startterm
      case (D.startterm p , CPF.complexityMeasure i) of
       ( Just D.Startterm_Full  , CPF.DerivationalComplexity ) -> return ()
       ( Just D.Startterm_Constructor_based , CPF.RuntimeComplexity ) -> return ()
       (s, m) -> Left $ text $ show (s,m)
    CPF.ACRewriteSystem {} -> do
      -- FIXME: absence of strategy, startterm
      let D.Signature fs = D.full_signature p
          mkid f = D.mk 0 (D.fs_name f) 
          as = S.fromList $ map mkid $ filter (\f -> D.fs_theory f `elem` [Just D.A, Just D.AC] ) fs
          cs = S.fromList $ map mkid $ filter (\f -> D.fs_theory f `elem` [Just D.C, Just D.AC] ) fs
          mustbeq msg s t =
            if s == t then return ()
            else Left $ vcat [ msg <+> "not equal" , pretty $ S.toList s, pretty $ S.toList t ]
      mustbeq "asymbols" as (S.fromList $ CPF.asymbols i)
      mustbeq "csymbols" cs (S.fromList $ CPF.csymbols i)
      return ()

matches claim p = case claim of
    NO -> case CPF.proof p of
        CPF.TrsNonterminationProof {} -> True
        CPF.RelativeNonterminationProof {} -> True
        _ -> False
    YES -> case CPF.proof p of
        CPF.TrsTerminationProof {} -> True
        CPF.RelativeTerminationProof {} -> True
        CPF.ACTerminationProof {} -> True
        _ -> False
    Bounds b -> case CPF.proof p of
        CPF.ComplexityProof {} -> C.matches b $ CPF.complexityClass $ CPF.input p
        _ -> False
    _ -> False
    

remove_timestamp :: String -> String
remove_timestamp = unwords . drop 1 . words

whine :: Bool -> [(String,String)] -> Doc -> IO ()
whine on_star_exec keyvals doc = do
    hPutStrLn stdout $ unlines $ map (\(k,v) -> k ++ "=" ++ show v) keyvals
    hFlush stdout
    if on_star_exec then System.Exit.exitSuccess else error $ show doc
    

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



