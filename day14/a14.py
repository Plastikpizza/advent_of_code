from a10 import knot_hash

def countUse(khash):
    n = int(khash, 16)
    i = 1
    s = 0
    while i < n:
        if i & n:
            s+=1
        i <<= 1
    return s

# FIXME: does not work, use michal's version...
def knotMapToArray(kmap):
    ans = []
    for line in kmap:
        lin = []
        n = int(line, 16)
        i = 1
        while i <= n:
            if i&n:
                lin.append(1)
            else:
                lin.append(0)
            i <<= 1
        while len(lin) < 128:
            lin.insert(0,0)
        ans.append(lin)
    return ans

def countPartitions(kmap):
    def done(k):
        a = 0
        for i in k:
            a += sum(i)
        return a == 0
    def findPartition(k):
        for y, l in enumerate(k):
            for x, v in enumerate(l):
                if v == 1:
                    return (x,y)
    def wipePartition(x, y, k):
        if -1 < x < len(k) and -1 < y < len(k):
            if k[y][x] > 0:
                k[y][x] = 0
                try:
                    wipePartition(x,   y-1, k)
                    wipePartition(x-1,   y, k)
                    wipePartition(x+1,   y, k)
                    wipePartition(x,   y+1, k)
                except IndexError:
                    pass
    i = 0
    for x in range(128):
        for y in range(128):
            if kmap[y][x] == 1:
                wipePartition(x, y, kmap)
                i += 1
    return i

if __name__ == '__main__':
    baseStr = "hfdlxzhv-"
    knotMap = []
    s = 0
    for i in range(0, 128):
        knotMap.append(knot_hash(baseStr+str(i)))
    for i, j in enumerate(knotMap):
        s += countUse(j)
    print(s)
    # ------------------------------------
    # -------- Thank you, Michco ---------
    # ------------------------------------
    key = "hfdlxzhv"
    bin_hashes = []
    for i in range(128):
        bin_hash = ""
        for x in knot_hash(key + '-' + str(i)):
            bin_hash += "{0:04b}".format(int(x, 16))
        bin_hashes.append([int(x) for x in bin_hash])
    # ------------------------------------
    # ------------------------------------
    # ------------------------------------
    print(countPartitions(bin_hashes))
