# TextClassify / txtcls

A haskell tf-idf implementation. This implemntation is for the ["Haskell text classification using Tf-Idf" blog post]( http://www.andrevdm.com/posts/2016-09-21-haskell-tfidf.html). 

## Building
```bash
git clone git@github.com:andrevdm/TextClassify.git
stack build
```

## Installing

```bash 
stack install
```

This will install txtcls into your local stack bin folder. 

# Usage Instructions

```bash
txtcls --help
```

```text
txtcls - Text Classifier. Version 0.1.2

Usage: txtcls --train TEXT [--input TEXT] [--parser TEXT] [--popts TEXT]
              [--clean TEXT]

Available options:
  -h,--help                Show this help text
  --train TEXT             Path to training data
  --input TEXT             Input file to categorise. If missing stdin will be
                           used
  --parser TEXT            Parser type, defaults to lines. Options are
                           lines/detail/csv
  --popts TEXT             Parser options
  --clean TEXT             Options name of text cleaner - see docs
```

# Usage examples

The examples folder contains scripts showing how txtcls can be used. The files are

 1. cleanText.sed - sed script for cleaning the words
 1. skipLines.awk  - awk script for skipping lines in the input CSV
 1. egLines.txt - example of data where each line is the data
 1. egCsv.csv - example of data in csv
 1. egCsvWithHeader.csv - example of data in CSV with a header text
 1. demoCsv.sh - run the example on egCsv.csv
 1. demoLines.sh - run the example on egLines.txt  
 1. demoDetail.sh - run the example on egLines.txt using the detail output 
 1. demoCsvWithHeader.sh - run the example on egCsvWithHeader.csv
 1. demoDetailInteractive.sh - run the detail parser interactively, read a line from stdin and write to stdout
 1. trainingData/cs.txt
 1. trainingData/hasekll.txt

## Lines

```txtcls --train ./trainingData --input egLines.txt --parser lines --clean ./cleanText.sed```

Where
 * ```--train ./trainingData``` is the path to the folder with the training data
 * ```--input egLines.txt``` is the data source to classify
 * ```--parser lines``` is the parser to use
 * ```--clean ./cleanText.sed``` is the external process or script to use to clean the text


## Detail

```txtcls --train ./trainingData --input egLines.txt --parser detail --clean ./cleanText.sed```

Where
 * ```--parser detail``` is the parser to use
 

## CSV

```txtcls --train ./trainingData --input egCsv.csv --parser csv --popts 2 --clean ./cleanText.sed | column -s , -t```

Where
 * ```--popts 2``` is column in the CSV data that contains the data to classify
 * ```| column -s , -t``` pipes the resulting CSV to column to display it as a table in the terminal window
 

## CSV with header text

```./skipLines.awk egCsvWithHeader.csv | txtcls --train ./trainingData --parser csv --popts 2 --clean ./cleanText.sed | column -s , -t```

Where
 * ```./skipLines.awk egCsvWithHeader.csv | ``` uses the awk script to remove 4 lines from the input CSV. Note that there is no ```--input``` paramter so the input is read from stdin (here the output of awk)

