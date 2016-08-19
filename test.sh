#!/bin/bash

stack install

ceta-postproc -n data/z25ok.cpf data/z25.xml
ceta-postproc -n data/z27.cpf data/z27.xml
ceta-postproc -n data/z042.cpf data/z042.xml

ceta-postproc -n data/AC28.cpf data/AC28.xml

ceta-postproc -n data/3.39.cpf data/3.39.xml
