{-
Author:  Christian Sternagel <c.sternagel@gmail.com> (2009-2015, 2017)
Author:  René Thiemann <rene.thiemann@uibk.ac.at> (2009-2014)
License: LGPL (see file COPYING.LESSER)
-}
module Main (main) where

import Ceta -- the certifier
import qualified Claim
import System.Environment -- for getArgs
import System.IO -- for file reading
import System.Exit -- for error codes

import qualified Data.List
import qualified Data.Char
import System.Clock
import Text.Printf

main = getArgs >>= \ args -> do
  (problemString, claim_proofString) <- case args of
    [ cpf, xtc ] -> do
      -- not on star-exec, don't need to strip intro
      problemString <- readProblemFile xtc
      claim_proofString <- readFile cpf
      return (problemString, claim_proofString)
    [ cpf, xtc, extradir ] -> do
      problemString <- readProblemFile xtc
      readFile cpf >>= \ s -> case strip_intro s of
        Nothing -> do
          terminate_with Nothing [ ("starexec-result", "MISSING-INTRO") ]
        Just claim_proofString -> do
          return (problemString, claim_proofString)
    _ -> error $ unlines
      [ "usage:"
      , "(on star-exec) ceta-postproc <cpf> <xtc> <extradir>"
      , "(at home)      ceta-postproc <cpf> <xtc>"
      , ""
      , "arguments for this call: " ++ show args
      ]
  case chop_claim claim_proofString of
    Nothing -> terminate_with Nothing [ ("starexec-result", "MAYBE") ]
    Just (claimString, proofString) -> do
        case Claim.parse claimString of
          Right Claim.MAYBE -> terminate_with Nothing [ ("starexec-result", "MAYBE") ]
          Right claim -> start False problemString claim proofString
          Left  _   -> terminate_with Nothing [ ("starexec-result", "INVALID-CLAIM:" ++ claimString) ] 

-- | Ceta cannot parse the problem statement for ITS termination, so it will ignore it (!)
-- https://github.com/jwaldmann/ceta-postproc/issues/16
readProblemFile :: FilePath -> IO (Maybe String)
readProblemFile fname =
  if Data.List.isSuffixOf ".smt2" fname
  then return Nothing
  else Just <$> readFile fname

start :: Bool -> Maybe String -> Claim.Claim -> String -> IO ()
start a problemString claim proofString = case cetaify claim of
  Nothing -> 
    terminate_with Nothing [ ("starexec-result", "UNSUPPORTED-CLAIM:" ++ show claim) ]
  Just cc -> do
    let (cr, mmsg) = case certify_proof a (explode <$> problemString) cc (explode proofString) of
           Certified  -> ("CERTIFIED", Nothing )
           Error message -> ("REJECTED", Just message)
           Unsupported message -> ("UNSUPPORTED", Just message)
    dt <- timed_whnf (length cr)
    let common = [ ("certification-result", cr)
                 , ("certification-time", format_time dt)
                 , ("output-size", show $ length proofString)
                 ]
    case mmsg of
      Nothing ->
        terminate_with mmsg $ ("starexec-result", show claim) : common
      Just msg ->
        terminate_with mmsg $ ("starexec-result", cr ++ "-" ++ show claim) : common

cetaify c = case c of
  Claim.YES -> Just $ Inl Terminating
  Claim.NO  -> Just $ Inl Nonterminating
  Claim.WORST_CASE Claim.None (Claim.Some (Claim.O d)) -> Just $ Inl $ Upperbound $ nat_of_integer d
  _ -> Nothing

terminate_with mmsg env = do
  hPutStrLn stdout $ unlines $ map (\(k,v) -> k ++ "=" ++ show v ) env
  hFlush stdout
  maybe (return ()) ( hPutStrLn stderr ) mmsg
  System.Exit.exitSuccess 

timed_whnf x = do
  start <- getTime Realtime
  end <- seq x $ getTime Realtime
  return $ toNanoSecs $ diffTimeSpec end start

format_time dt =
  printf "%.1f" $ ( fromIntegral dt * 1e-9 :: Double )

separator_length = 80
separator = replicate separator_length '-'

strip_intro s = case Data.List.stripPrefix separator s of
  Nothing -> case s of
    [] -> Nothing
    _ : s' -> strip_intro s'
  Just rest -> Just rest
  
chop_claim s = case dropWhile Data.Char.isSpace s of
  [] -> Nothing
  s' -> Just $ span (/= '\n') s'
