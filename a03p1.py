# cheers to Martin Javorka for the algorithm :)
import sys

def found(v, x, y):
    print("found {} at ({},{})".format(v, x, y))
    print("cost: {}".format(abs(x)+abs(y)))
    exit()

def main(search):
    x = 1
    y = 0
    v = 2
    horizontal = 2
    vertical   = 1
    sign       = 1

    while True:
        for i in range(vertical):
            if v == search:
                found(v, x, y)
            y   += 1 * sign
            v   += 1
        vertical += 1
        sign *= -1
        for i in range(horizontal):
            if v == search:
                found(v, x, y)
            x   += 1 * sign
            v   += 1
        horizontal += 1

if __name__ == '__main__':
    main(int(sys.argv[1]))
