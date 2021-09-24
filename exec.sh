#!/bin/bash

mkdir data
mv $1 data/.

sudo apt install pandoc
R -e "rmarkdown::render('report/desempenhoHistorico.Rmd',output_file='output.html')"

mv report/output.html .