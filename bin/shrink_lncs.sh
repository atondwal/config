#!/bin/bash

pdfcrop --margins "5 10 5 20" --clip $1 crop.pdf
pdfnup --nup 2x1 crop.pdf
mv crop-nup.pdf $1
rm crop.pdf
