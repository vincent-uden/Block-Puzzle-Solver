from copy import copy

class Grid:
    def __init __(self, w, h, grd=None):
        self.grid = [[str(0) for x in range(w)] for y in range(h)]
        self.w = w
        self.h = h
        if grd != None:
            self.grid = grd

    def __str__(self):
        output = ""
        for row in self.grid:
            for x in row:
                output += str(x) + " "
            output += "\n"
        return output[:-1]

    def place(self, shape, x, y):
        print("_________")
        #print(self)
        tmp_grid = self.grid_copy()
        print(tmp_grid)
        print("\n###########################")
        print(f"#### Kernel - {x}, {y} ####")
        print(self.get_kernel(x, y, len(shape[0]), len(shape)))
        for iy in range(0, len(shape)):
            for ix in range(0, len(shape[0])):
                if shape[iy][ix] != "0":
                    if tmp_grid[iy + y][ix + x] != "0":
                        print("!!!!!!!!!!!!!!!!RAISING EXCEPTION")
                        raise Exception(f"Cant place on non-empty pos ({iy + y},{ix + x})")
                        return
                    else:
                        tmp_grid[iy + y][ix + x] = shape[iy][ix]
        self.grid = tmp_grid[::]
        print("Placing")

    def get_kernel(self, x, y, w, h):
        output = []
        for row in range(y, y + h):
            r = self.grid[row]
            r = r[x:x + w]
            output.append(r)
        return output

    def grid_copy(self):
        output = []
        for row in self.grid:
            r = []
            for item in row:
                r.append(item)
            output.append(r)
        return output

class Block:
    def __init__(self, shape: list, symbol: str = "1"):
        self.shape = shape
        self.symbol = symbol
        self.w = len(self.shape[0])
        self.h = len(self.shape)
        if self.symbol == "0":
            raise Exception("Symbol can't be 0")

    def __str__(self):
        data = self.get_shape()
        output = "\n".join([" ".join(row) for row in data])
        return output

    def get_shape(self):
        output = []
        for row in self.shape:
            irow = []
            for thing in row:
                if thing:
                    irow.append(self.symbol)
                else:
                    irow.append("0")
            output.append(irow[::])
        return output

    def get_rotation(self, turns):
        rotated = self.shape[::]
        for i in range(turns):
            rotated = zip(*rotated[::-1])
            rotated = list(rotated)
            rotated = [list(thing) for thing in rotated]
        return rotated
    
    def rotate(self, turns):
        self.shape = self.get_rotation(turns)
        self.w = len(self.shape[0])
        self.h = len(self.shape)

def gen_2d_blocks():
    t = Block([[1, 1, 1], [0, 1, 0]], "T")
    L = Block([[1, 0], [1, 0], [1, 1]], "L")
    l = Block([[0, 1], [0, 1], [1, 1]], "˩")
    s = Block([[1, 0], [1, 1], [0, 1]], "S")
    z = Block([[0, 1], [1, 1], [1, 0]], "Z")
    o = Block([[1, 1], [1, 1]], "O")
    i = Block([[1], [1], [1], [1]], "I")
    return [t, L, l, s, z, o, i]

def block_set1():
    [t, L, l, s, z, o, i] = gen_2d_blocks()
    output = []
    output.append(copy(t))
    output.append(copy(t))
    output.append(copy(l))
    output.append(copy(i))
    output[0].symbol = "t"
    return output

def solve_set1():
    grid = Grid(4, 4)
    blocks = block_set1()
    grid.place(blocks[0].get_shape(), 0, 0)
    blocks[1].rotate(1)
    grid.place(blocks[1].get_shape(), 2, 0)
    blocks[2].rotate(1)
    grid.place(blocks[2].get_shape(), 0, 1)
    blocks[3].rotate(1)
    grid.place(blocks[3].get_shape(), 0, 3)
    return grid

def put_block(grid, block):
    my_block = copy(block)
    for r in range(0, 4):
        for y in range(grid.h - block.h + 1):
            for x in range(grid.w - block.w + 1):
                try:
                    my_block.rotate(r)
                    print("_______")
                    print(f"X:{x} Y:{y}")
                    print(my_block)
                    grid.place(my_block.get_shape(), x, y)
                    return
                except:
                    pass

def brute_force(grid, blocks, start=0):
    if start >= len(blocks):
        return []
    my_block = copy(blocks[start])
    solutions = []
    for r in range(0, 4):
        for y in range(grid.h - my_block.h + 1):
            for x in range(grid.w - my_block.w + 1):
                try:
                    my_block.rotate(r)
                    print("_______")
                    print(f"X:{x} Y:{y}")
                    print(my_block)
                    grid.place(my_block.get_shape(), x, y)
                    solutions += brute_force(Grid(grid.w, grid.h, grd=grid.grid_copy()), blocks, start + 1)
                    # Returnera alla följande lösningar
                    return
                except:
                    pass

or r in range(0, 4):
        for y in range(

grid = Grid(4, 4)
blocks = block_set1()
for block in blocks:
    print(block)
put_block(grid, blocks[0])
put_block(grid, blocks[1])
put_block(grid, blocks[2])
put_block(grid, blocks[3])

print(grid)
