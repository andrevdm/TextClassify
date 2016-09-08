#!/bin/sed -uf
s/c\*/ /gi
s/jan\|feb\|mar\|apr\|may\|jun\|jul\|aug\|sep\|oct\|nov\|dec/ /gi
s/ \+$//gi
s/[0-9]\{2\}$//gi
s/\[\(\)!\-\/*\\\]/ /g
s/[*\/\(\)_,\]/ /g
s/-/ /g
s/\t/ /g
s/  \+/ /g
s/ \+$//gi
s/^ \+//gi
