# advent of code 2019 day 3 implementation in python3

# implementation details:
#   central port is (0, 0)

# find the manhattan distance between two points
def distance(a, b):
    return abs(a[0]-b[0]) + abs(a[1]-b[1])

# pathfind
class Grid:
    def __init__(self):
        #self.grid = [[0 for x in range(10000)] for y in range(10000)]  
        self.grid = [[0 for x in range(500)] for y in range(500)]
        self.intersections = set()

    def line(self, origin, dest):
        if origin[0] == dest[0]:
            # moving along the y axis
            for i in range(origin[1], dest[1]+1):
                if self.grid[origin[0]][i] == ".":
                    self.intersections.add((origin[0], i))
                    self.grid[origin[0]][i] = "%"
                elif self.grid[origin[0]][i] == "%":
                    continue
                else:
                    self.grid[origin[0]][i] = "."
        else:
            # moving along x axis
            for i in range(origin[0], dest[0]+1):
                if self.grid[i][origin[1]] == ".":
                    self.intersections.add((i, origin[1]))
                    self.grid[i][origin[1]] = "%"
                elif self.grid[i][origin[1]] == "%":
                    continue
                else:
                    self.grid[i][origin[1]] = "."
        return dest


    def execute_paths(self, path):
        # extract the wire one and two paths from the path
        wirepaths = path.splitlines()
        
        # make it extensible
        for i in range(0, len(wirepaths)):
            wirepaths[i] = wirepaths[i].split(",")

        # now iterate and find intersections
        for path in wirepaths:
            coords = (0, 0)
            for command in path:
                dest = (0, 0)
                print(f"executing command={command}, coordinates={coords}")
                if command[0] == "L":
                    dest = (coords[0] + -int(command[1:len(command)]), coords[1])
                elif command[0] == "R":
                    dest = (coords[0] + int(command[1:len(command)]), coords[1])
                elif command[0] == "U":
                    dest = (coords[0], coords[1] + int(command[1:len(command)]))
                elif command[0] == "D":
                    dest = (coords[0], coords[1] + -int(command[1:len(command)]))
                coords = self.line(coords, dest)

    def get_closest_intersection(self):
        # iterate over all intersections and get the longest
        longest = 0
        for isect in self.intersections:
            # get the distance
            dist = distance((0, 0), isect)
            if dist > longest:
                longest = dist
        # return the longest
        return longest



# fill in the data
grid = Grid()

grid.execute_paths("""R75,D30,R83,U83,L12,D49,R71,U7,L72
        U62,R66,U55,R34,D71,R55,D58,R83""")

print(grid.get_closest_intersection())
