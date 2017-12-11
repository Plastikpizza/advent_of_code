import sys
import copy

instructions = []
for line in sys.stdin:
    instructions.append(int(line))
instructions_copy = copy.copy(instructions)

# part 1
index = 0
counter = 0
while index < len(instructions) and index >= 0:
    instructions[index] += 1
    index += instructions[index]-1
    counter += 1
print(counter)

# part 2
index = 0
counter = 0
while index < len(instructions_copy) and index >= 0:
    if instructions_copy[index] > 2:
        instructions_copy[index] -= 1
        index += instructions_copy[index]+1
    else:
        instructions_copy[index] += 1
        index += instructions_copy[index]-1
    counter += 1
print(counter)
