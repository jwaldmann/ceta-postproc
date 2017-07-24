{-
Author:  Christian Sternagel <c.sternagel@gmail.com> (2009-2015, 2017)
Author:  René Thiemann <rene.thiemann@uibk.ac.at> (2009-2014)
License: LGPL (see file COPYING.LESSER)
-}
module Main (main) where

import Ceta -- the certifier
import System.Environment -- for getArgs
import System.IO -- for file reading
import System.Exit -- for error codes

main = getArgs >>= parse False

parse _ ("--version":_) = do putStrLn version
parse _ ("--allow-assumptions":args) = parse True args
parse flag [xtc,cpf] = do
  problemString <- readFile xtc
  proofString <- readFile cpf
  start flag (Just problemString) proofString

parse flag [cpf] = do
  problemString <- readFile cpf
  start flag Nothing problemString

parse _ _ = do
  hPutStrLn stderr usage
  exitWith (ExitFailure 4)

start a problemString proofString = 
    do case certify_proof a problemString proofString of
         Sumbot (Inr (Certified prf)) -> 
             do putStrLn ("CERTIFIED " ++ prf)
                exitWith ExitSuccess
         Sumbot (Inr (Error message)) -> 
             do putStrLn "REJECTED"
                hPutStrLn stderr message
                exitWith (ExitFailure 1)
         Sumbot (Inr (Unsupported message)) -> 
             do putStrLn "UNSUPPORTED" 
                hPutStrLn stderr message
                exitWith (ExitFailure 2)
         Sumbot (Inl message) -> 
             do putStrLn "UNKNOWN ERROR"
                hPutStrLn stderr message
                exitWith (ExitFailure 3)

usage = "usage: ceta [[--allow-assumptions] certificate | --version]\n\
        \  \n\
        \  A \"certificate\" is an XML file in certification problem format (CPF).\n\
        \  \n\
        \  --allow-assumptions    Allow (axiomatic) assumptions in the certificate.\n\
        \  --version              Print the version number (+ mercurial id)."
