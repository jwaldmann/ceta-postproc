{-
Author:  Christian Sternagel <c.sternagel@gmail.com> (2009-2015, 2017)
Author:  René Thiemann <rene.thiemann@uibk.ac.at> (2009-2014)
Author:  Akihisa Yamada <akihisa.yamada@uibk.ac.at> (2017)
License: LGPL (see file COPYING.LESSER)
-}
module Main (main) where

import Ceta -- the certifier
import System.Environment -- for getArgs
import System.IO -- for file reading
import System.Exit -- for error codes

main = getArgs >>= parse False

parse _ ("--version":_) = do putStrLn Main.version
parse _ ("--allow-assumptions":args) = parse True args
parse flag [xtc,claim,cpf] = do
  problemString <- readFile xtc
  claimString <- readFile claim
  proofString <- readFile cpf
  start flag (Just problemString) (Inr claimString) proofString

parse flag [cpf] = do
  problemString <- readFile cpf
  start flag Nothing (Inl Anything) problemString

parse _ _ = do
  hPutStrLn stderr usage
  exitWith (ExitFailure 4)

start a problemString claimString proofString = 
    do case certify_proof a problemString claimString proofString of
         Certified -> 
             do putStrLn ("CERTIFIED")
                exitWith ExitSuccess
         Error message -> 
             do putStrLn "REJECTED"
                hPutStrLn stderr message
                exitWith (ExitFailure 1)
         Unsupported message -> 
             do putStrLn "UNSUPPORTED" 
                hPutStrLn stderr message
                exitWith (ExitFailure 2)

usage = "usage: ceta [[--allow-assumptions] certificate | --version]\n\
        \  \n\
        \  A \"certificate\" is an XML file in certification problem format (CPF).\n\
        \  \n\
        \  --allow-assumptions    Allow (axiomatic) assumptions in the certificate.\n\
        \  --version              Print the version number."
version = "2.31"
