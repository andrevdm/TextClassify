#!/bin/sh
./skipLines.awk egCsvWithHeader.csv | txtcls --train ./trainingData --parser csv --popts 2 --clean ./cleanText.sed | column -s , -t
