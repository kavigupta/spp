import fileinput

with open("temp2", "w") as f:
    for line in fileinput.input():
        f.write(line)
