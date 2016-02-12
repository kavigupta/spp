import fileinput

with open("temp1", "r") as f:
    print(f.readContents())

for line in fileinput.input():
    line = line[:-1]
    print(line.reverse())
