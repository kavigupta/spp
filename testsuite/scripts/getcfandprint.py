#!/usr/bin/python3

import fileinput

with open(".spp-current-file") as f:
    print(f.read()[-8:])

for line in fileinput.input():
    print(line, end="")
