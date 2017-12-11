import copy

def redistribute(banks):
    maxIndex = 0
    maximum  = banks[0]
    for index, entries in enumerate(banks):
        if maximum < entries:
            maximum = entries
            maxIndex = index
    banks[maxIndex] = 0
    while maximum > 0:
        maxIndex += 1
        maxIndex %= len(banks)
        banks[maxIndex] += 1
        maximum -= 1
    return banks

seenConstellations = []
constellation = [2,8,8,5,4,2,3,1,5,5,1,2,15,13,5,14]
while not constellation in seenConstellations:
    seenConstellations.append(copy.copy(constellation))
    constellation = redistribute(constellation)
# part one
print(len(seenConstellations))
i = 1
for con in reversed(seenConstellations):
    if constellation == con:
        # part two
        print(i)
        break
    i += 1
