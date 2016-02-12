import fileinput

with open("temp1", "r") as f:
    print(f.read())

for line in fileinput.input():
    line = line[:-1]
    print(line[::-1])
