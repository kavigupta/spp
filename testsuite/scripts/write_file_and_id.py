#!/usr/bin/python3
import fileinput


with open(".spp-current-file") as f:
    loc = open(f.read() + "-temp", "w")

for line in fileinput.input():
    line = line[:-1]
    print(line)
    loc.write(line + "\n")
