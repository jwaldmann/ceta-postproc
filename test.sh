#!/bin/bash

stack install

ceta-postproc -n data/z025ok.cpf data/z025.xml
ceta-postproc -n data/z027.cpf data/z027.xml
ceta-postproc -n data/z042.cpf data/z042.xml

ceta-postproc -n data/AC28.cpf data/AC28.xml

