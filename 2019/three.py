# advent of code 2019 day 3 implementation in python3
# takes paths as space separated strings of comma-separated instructions as arguments

import sys

# find the manhattan distance between two points
def distance(a, b):
    return abs(a[0]-b[0]) + abs(a[1]-b[1])

# calculate the distance it takes to get to a point along paths
def point_to_distance(paths, point):
    dist = 0
    for path in paths:
        instructions = path.split(",")
        pos = (0, 0)
        for inst in instructions:
            inst = inst.strip()
            if inst[0] == "L":
                # left
                target = pos[0] - int(inst[1:len(inst)])
                for i in range(pos[0], target, -1):
                    if (i, pos[1]) == point:
                        break
                    dist += 1
                else:
                    pos = (target, pos[1])
                    continue
                break
            elif inst[0] == "R":
                # right
                target = pos[0] + int(inst[1:len(inst)])
                for i in range(pos[0], target, 1):
                    if (i, pos[1]) == point:
                        break
                    dist += 1
                else:
                    pos = (target, pos[1])
                    continue
                break
            elif inst[0] == "U":
                # up
                target = pos[1] + int(inst[1:len(inst)])
                for i in range(pos[1], target, 1):
                    if (pos[0], i) == point:
                        break
                    dist += 1
                else:
                    pos = (pos[0], target)
                    continue
                break
            elif inst[0] == "D":
                # down
                target = pos[1] - int(inst[1:len(inst)])
                for i in range(pos[1], target, -1):
                    if (pos[0], i) == point:
                        break
                    dist += 1
                else:
                    pos = (pos[0], target)
                    continue
                break
    return dist

# convert a path sequence to a set of points
def path_to_points(path):
    instructions = path.split(",")
    coordinates = set()
    pos = (0, 0)
    for inst in instructions:
        inst = inst.strip()
        if inst[0] == "L":
            # left
            target = pos[0] - int(inst[1:len(inst)])
            for i in range(pos[0], target, -1):
                coordinates.add((i, pos[1]))
            pos = (target, pos[1])
        elif inst[0] == "R":
            # right
            target = pos[0] + int(inst[1:len(inst)])
            for i in range(pos[0], target, 1):
                coordinates.add((i, pos[1]))
            pos = (target, pos[1])
        elif inst[0] == "U":
            # up
            target = pos[1] + int(inst[1:len(inst)])
            for i in range(pos[1], target, 1):
                coordinates.add((pos[0], i))
            pos = (pos[0], target)
        elif inst[0] == "D":
            # down
            target = pos[1] - int(inst[1:len(inst)])
            for i in range(pos[1], target, -1):
                coordinates.add((pos[0], i))
            pos = (pos[0], target)
    return coordinates

# get a list of all of the coordinates of each path
coordinate_list = []
for path in sys.argv[1:]:
    coordinate_list.append(path_to_points(path))

# get all of the common points
intersections = set.intersection(*coordinate_list)

# get the shortest distance from origin
shortest_distance_direct_from_origin = 0
for p in intersections:
   dist = distance((0, 0), p)
   if dist == 0:
       continue
   if shortest_distance_direct_from_origin == 0 or dist < shortest_distance_direct_from_origin:
       shortest_distance_direct_from_origin = dist

# display it
print(f"shortest (direct from origin)={shortest_distance_direct_from_origin}")

# get a distance to all of the common points
shortest_distance_from_origin = 0
for p in intersections:
    dist = point_to_distance(sys.argv[1:], p)
    if dist == 0:
        continue
    if shortest_distance_from_origin == 0 or dist < shortest_distance_from_origin:
        shortest_distance_from_origin = dist

# display this one, too
print(f"shortest (along paths)={shortest_distance_from_origin}")
