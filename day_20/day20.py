from functools import reduce
import numpy as np
import operator

tile_text = """Tile 2311:
..##.#..#.
##..#.....
#...##..#.
####.#...#
##.##.###.
##...#.###
.#.#.#..##
..#....#..
###...#.#.
..###..###"""


class Tile:
    def __init__(self, tile_text="") -> None:
        rows = tile_text.split("\n")
        self.number = rows[0].replace("Tile ", "").replace(":", "")
        tile_content = [
            [1 if character == "#" else 0 for character in row] for row in rows[1:]
        ]

        self.content = np.array(tile_content)
        self.left = None
        self.top = None
        self.right = None
        self.down = None
        self.to_test = []
        self.x_index = None
        self.y_index = None
        self.parent = None

    # def __repr__(self) -> str:
    #     return f"Tile {self.number} \n{self.content.__str__()}\n"

    def __repr__(self) -> str:
        return self.number

    def is_full(self):
        self.left and self.top and self.right and self.down


print(Tile(tile_text))

print(Tile(tile_text).content[0, :])
print(Tile(tile_text).content[-1, :])
print(Tile(tile_text).content[:, 0])
print(Tile(tile_text).content[:, -1])


all_tiles_text = """Tile 2311:
..##.#..#.
##..#.....
#...##..#.
####.#...#
##.##.###.
##...#.###
.#.#.#..##
..#....#..
###...#.#.
..###..###

Tile 1951:
#.##...##.
#.####...#
.....#..##
#...######
.##.#....#
.###.#####
###.##.##.
.###....#.
..#.#..#.#
#...##.#..

Tile 1171:
####...##.
#..##.#..#
##.#..#.#.
.###.####.
..###.####
.##....##.
.#...####.
#.##.####.
####..#...
.....##...

Tile 1427:
###.##.#..
.#..#.##..
.#.##.#..#
#.#.#.##.#
....#...##
...##..##.
...#.#####
.#.####.#.
..#..###.#
..##.#..#.

Tile 1489:
##.#.#....
..##...#..
.##..##...
..#...#...
#####...#.
#..#.#.#.#
...#.#.#..
##.#...##.
..##.##.##
###.##.#..

Tile 2473:
#....####.
#..#.##...
#.##..#...
######.#.#
.#...#.#.#
.#########
.###.#..#.
########.#
##...##.#.
..###.#.#.

Tile 2971:
..#.#....#
#...###...
#.#.###...
##.##..#..
.#####..##
.#..####.#
#..#.#..#.
..####.###
..#.#.###.
...#.#.#.#

Tile 2729:
...#.#.#.#
####.#....
..#.#.....
....#..#.#
.##..##.#.
.#.####...
####.#.#..
##.####...
##..#.##..
#.##...##.

Tile 3079:
#.#.#####.
.#..######
..#.......
######....
####.#..#.
.#...#.##.
#.#####.##
..#.###...
..#.......
..#.###..."""

all_tiles = [Tile(tile_text) for tile_text in all_tiles_text.split("\n\n")]
# print(all_tiles)

oriented_tile = all_tiles[-1]
start_tile = oriented_tile
oriented_tile.x_index = 0
oriented_tile.y_index = 0


def findNext(oriented_tile, available_tiles):
    for tile in available_tiles:
        if not oriented_tile.left:
            left_border = oriented_tile.content[:, 0]
            for rotation in range(0, 4):
                for flip in range(0, 2):
                    rotated_tile = np.rot90(tile.content, rotation)
                    if flip:
                        rotated_tile = np.fliplr(rotated_tile)
                    right_border = rotated_tile[:, -1]
                    if np.array_equal(left_border, right_border):
                        tile.right = oriented_tile
                        tile.content = rotated_tile
                        tile.y_index = oriented_tile.y_index
                        tile.x_index = oriented_tile.x_index - 1
                        oriented_tile.left = tile
                        oriented_tile.to_test.append(tile)
                        tile.parent = oriented_tile

                        return (
                            oriented_tile,
                            [
                                available_tile
                                for available_tile in available_tiles
                                if available_tile.number != tile.number
                            ],
                            True,
                        )
        if not oriented_tile.right:
            right_border = oriented_tile.content[:, -1]
            for rotation in range(0, 4):
                for flip in range(0, 2):
                    rotated_tile = np.rot90(tile.content, rotation)
                    if flip:
                        rotated_tile = np.fliplr(rotated_tile)
                    left_border = rotated_tile[:, 0]
                    if np.array_equal(left_border, right_border):

                        tile.left = oriented_tile
                        tile.content = rotated_tile
                        oriented_tile.right = tile
                        tile.y_index = oriented_tile.y_index
                        tile.x_index = oriented_tile.x_index + 1
                        oriented_tile.to_test.append(tile)
                        tile.parent = oriented_tile

                        return (
                            oriented_tile,
                            [
                                available_tile
                                for available_tile in available_tiles
                                if available_tile.number != tile.number
                            ],
                            True,
                        )
        if not oriented_tile.top:
            top_border = oriented_tile.content[0, :]
            for rotation in range(0, 4):
                for flip in range(0, 2):
                    rotated_tile = np.rot90(tile.content, rotation)
                    if flip:
                        rotated_tile = np.fliplr(rotated_tile)
                    down_border = rotated_tile[-1, :]
                    if np.array_equal(top_border, down_border):
                        tile.down = oriented_tile
                        tile.content = rotated_tile

                        tile.y_index = oriented_tile.y_index + 1
                        tile.x_index = oriented_tile.x_index
                        oriented_tile.top = tile
                        oriented_tile.to_test.append(tile)
                        tile.parent = oriented_tile

                        return (
                            oriented_tile,
                            [
                                available_tile
                                for available_tile in available_tiles
                                if available_tile.number != tile.number
                            ],
                            True,
                        )
        if not oriented_tile.down:
            down_border = oriented_tile.content[-1, :]
            for rotation in range(0, 4):
                for flip in range(0, 2):
                    rotated_tile = np.rot90(tile.content, rotation)
                    if flip:
                        rotated_tile = np.fliplr(rotated_tile)
                    top_border = rotated_tile[0, :]
                    if np.array_equal(top_border, down_border):
                        tile.top = oriented_tile
                        tile.content = rotated_tile
                        tile.y_index = oriented_tile.y_index - 1
                        tile.x_index = oriented_tile.x_index
                        oriented_tile.down = tile

                        oriented_tile.to_test.append(tile)
                        tile.parent = oriented_tile

                        return (
                            oriented_tile,
                            [
                                available_tile
                                for available_tile in available_tiles
                                if available_tile.number != tile.number
                            ],
                            True,
                        )
    return (oriented_tile, available_tiles, False)


# print(findNext(oriented_tile, all_tiles[1:]))

available_tiles = all_tiles[:-1]

while available_tiles:
    oriented_tile, available_tiles, found = findNext(oriented_tile, available_tiles)
    if found:
        continue

    if oriented_tile.to_test:
        oriented_tile = oriented_tile.to_test.pop()
        continue

    # if oriented_tile.left and not oriented_tile.left.tested:
    #     oriented_tile = oriented_tile.left
    #     continue

    # if oriented_tile.top and not oriented_tile.top.tested:
    #     oriented_tile = oriented_tile.top
    #     continue

    # if oriented_tile.right and not oriented_tile.right.tested:
    #     oriented_tile = oriented_tile.right
    #     continue

    # if oriented_tile.down and not oriented_tile.down.tested:
    #     oriented_tile = oriented_tile.down
    #     continue
    oriented_tile = start_tile.to_test.pop()


# findNext(available_tiles[1], [available_tiles[4]])

# print(oriented_tile)


def find_top_left(oriented_tile):
    left_most = oriented_tile
    while left_most.left:
        left_most = left_most.left
    top_most = left_most
    while top_most.top:
        top_most = top_most.top
    return top_most


def find_top_right(oriented_tile):
    right = oriented_tile
    while right.right:
        right = right.right
    top_most = right
    while top_most.top:
        top_most = top_most.top
    return top_most


def find_down_left(oriented_tile):
    left_most = oriented_tile
    while left_most.left:
        left_most = left_most.left
    down = left_most
    while down.down:
        down = down.down
    return down


def find_down_right(oriented_tile):
    down = oriented_tile
    while down.down:
        down = down.down
    right = down
    while right.right:
        right = right.right

    return right


import math


def tabularize(all_tiles):
    min_x = min([tile.x_index for tile in all_tiles])
    min_y = min([tile.y_index for tile in all_tiles])

    for tile in all_tiles:
        tile.x_index -= min_x
        tile.y_index -= min_y

    side = int(math.sqrt(len(all_tiles)))
    return [
        [
            next(tile for tile in all_tiles if tile.x_index == x and tile.y_index == y)
            for x in range(0, side)
        ]
        for y in range(0, side)
    ]


print(tabularize(all_tiles))


def find_corners(tabularized_tiles):
    return (
        float(tabularized_tiles[0][0].number),
        float(tabularized_tiles[0][-1].number),
        float(tabularized_tiles[-1][0].number),
        float(tabularized_tiles[-1][-1].number),
    )


print(reduce(operator.mul, find_corners(tabularize(all_tiles))))


def solve(tiles_text):
    all_tiles = [Tile(tile_text) for tile_text in tiles_text.split("\n\n")]
    # print(all_tiles)

    oriented_tile = all_tiles[-1]
    start_tile = oriented_tile
    oriented_tile.x_index = 0
    oriented_tile.y_index = 0

    available_tiles = all_tiles[:-1]

    while available_tiles:
        print(len(available_tiles))
        oriented_tile, available_tiles, found = findNext(oriented_tile, available_tiles)
        if found:
            continue

        if oriented_tile.to_test:
            oriented_tile = oriented_tile.to_test.pop()
            continue

        if oriented_tile.left and oriented_tile.left.to_test:
            oriented_tile = oriented_tile.left.to_test.pop()
            continue

        if oriented_tile.top and oriented_tile.top.to_test:
            oriented_tile = oriented_tile.top.to_test.pop()
            continue

        if oriented_tile.right and oriented_tile.right.to_test:
            oriented_tile = oriented_tile.right.to_test.pop()
            continue

        if oriented_tile.down and oriented_tile.down.to_test:
            oriented_tile = oriented_tile.down.to_test.pop()
            continue
        oriented_tile = oriented_tile.parent
    print(reduce(operator.mul, find_corners(tabularize(all_tiles))))


with open("input_20.txt", "r") as fp:
    input = fp.read()
    solve(input)

# print(find_top_left(start_tile).number)
# print(find_top_right(start_tile).number)
# print(find_down_right(start_tile).number)
# print(find_down_left(start_tile).number)

# print(find_top_left(oriented_tile).number)
# print(find_top_right(oriented_tile).number)
# print(find_down_right(oriented_tile).number)
# print(find_down_left(oriented_tile).number)


# def left_part(oriented_tile):
#     while oriented_tile.left:
#         yield oriented_tile.left
#         oriented_tile = oriented_tile.left


# def right_part(oriented_tile):
#     while oriented_tile.right:
#         yield oriented_tile.right
#         oriented_tile = oriented_tile.right


# def row(oriented_tile):
#     return [
#         list(left_part(oriented_tile)).reverse(),
#         oriented_tile,
#         list(right_part(oriented_tile)),
#     ]


# print(row(oriented_tile))
# print(row(oriented_tile.down))

