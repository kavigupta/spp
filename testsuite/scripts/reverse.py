#!/usr/bin/python3
import fileinput

for line in fileinput.input():
    line = line[:-1]
    print(line[::-1])
