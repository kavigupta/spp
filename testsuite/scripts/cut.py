#!/usr/bin/python3
import fileinput


for line in fileinput.input():
    line = line[:-1]
    if len(line) > 4:
        line = line[4:]
    print(line)
