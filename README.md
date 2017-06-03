# Lao Wiktionary Parser

## Prerequisite

1. SBCL
2. Quicklisp
3. wget

## Install

````
sbcl --load install.lisp --quit
````

## Usage

````
wget -O - https://dumps.wikimedia.org/lowiktionary/latest/lowiktionary-latest-pages-articles.xml.bz2 | bzcat > lowik.xml
sbcl --noinform --load parse.lisp --eval '(extract "lowik.xml")' --quit > lowik.lisp
````