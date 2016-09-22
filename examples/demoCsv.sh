#!/bin/sh
txtcls --train ./trainingData --input egCsv.csv --parser csv --popts 2 --clean ./cleanText.sed | column -s , -t
