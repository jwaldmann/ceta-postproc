ceta-postproc
=============

CeTA http://cl-informatik.uibk.ac.at/software/ceta/ 
is a tool that certifies (non)termination or (non)confluence 
or completion or complexity proofs.

This project contains a version of CeTA that can be used as a "post-processor" 
on starexec, https://wiki.uiowa.edu/display/stardev/User+Guide#UserGuide-Post-Processors

It is called with two arguments:
* first: name of file containing solver's output: 
** status (YES/NO/..) in first line
** followed by an XML document in CPF format
** each line has a timestamp prepended 
* second: name of file containing the benchmark.

CeTA (C) Christian Sternagel and Rene Thiemann 2009-2013,
postprocessor modifications (C) Johannes Waldmann 2014