import fileinput
import os
from sys import argv

if argv[1] == '-':
    write = print
else:
    f = open(argv[1], "a")
    write = lambda st: f.write(st + "\n")    

with open(".spp-current-file") as cf:
    write(cf.read())
write(os.getcwd()[-16:])

for _ in fileinput.input():
    pass
