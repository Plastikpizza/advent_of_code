from sys import stdin

class Direction(object):
    down  = 0
    up    = 1
    left  = 2
    right = 3

class Train(object):
    x = 0
    y = 0
    d = Direction.down
    c = []

    def __str__(self):
        d = "down"
        if self.d == Direction.up:
            d = "up"
        elif self.d == Direction.left:
            d = "left"
        elif self.d == Direction.right:
            d = "right"
        return "Train at {} {} going {}".format(self.x, self.y, d)

    def read(self, string):
        return string[self.y][self.x]

    def easyDrive(self):
        if self.d == Direction.down:
            self.y += 1
        elif self.d == Direction.up:
            self.y -= 1
        elif self.d == Direction.right:
            self.x += 1
        elif self.d == Direction.left:
            self.x -= 1

    def drive(self, string):
        char = self.read(string)
        if char == "|" or char == "-":
            self.easyDrive()
        elif char == "+":
            if self.d in [Direction.up, Direction.down]:
                if string[self.y][self.x+1] == "-":
                    self.d = Direction.right
                    self.x += 1
                elif string[self.y][self.x-1] == "-":
                    self.d = Direction.left
                    self.x -= 1
            else:
                if string[self.y+1][self.x] == "|":
                    self.d = Direction.down
                    self.y += 1
                elif string[self.y-1][self.x] == "|":
                    self.d = Direction.up
                    self.y -= 1
        else:
            self.c.append(char)
            self.easyDrive()

if __name__ == '__main__':
    network = []
    for line in stdin:
        network.append(line)
    p = Train()
    # finding the entry-point
    p.x = [i for i in range(len(network[0])) if network[0][i] == "|"][0]
    i = 0
    while len(p.c) < 9:
        p.drive(network)
        i += 1
    # part uno
    print(p.c)
    # part due
    print(i)
# who reads this, after all...?
