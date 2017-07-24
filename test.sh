#!/bin/bash

stack install

cepp () {
    echo ================= $1 $2
    ceta-postproc --nostar $1 $2
}

echo "must work:"

cepp data/z25ok.cpf data/z25.xml
cepp data/z27.cpf data/z27.xml
cepp data/z042.cpf data/z042.xml
cepp data/AC28.cpf data/AC28.xml
cepp data/3.39.cpf data/3.39.xml
cepp data/AG01-3.24.cpf data/AG01-3.24.xml

echo "must fail:"

cepp data/z25wrong.cpf data/z25.xml
