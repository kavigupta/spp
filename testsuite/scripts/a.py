import fileinput

f = open("temp1", "w")

for line in fileinput.input():
    f.write(line)
    line = line[:-1]
    if len(line) > 4:
        line = line[4:]
    print(line)
