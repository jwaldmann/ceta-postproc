#!/bin/bash

stack install

cepp () {
    cat $1 | tail -n+2 | ceta-postproc $2 /dev/stdin
}

cepp data/z25ok.cpf data/z25.xml
cepp data/z27.cpf data/z27.xml
cepp data/z042.cpf data/z042.xml
cepp data/AC28.cpf data/AC28.xml
cepp data/3.39.cpf data/3.39.xml
cepp data/AG01-3.24.cpf data/AG01-3.24.xml
