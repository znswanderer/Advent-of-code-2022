# -*- coding: utf-8 -*-
"""Advent of Code, Day 16

for part2 use:

>>> w = day16.World("input", tmax=26)
>>> day16.optimize2_b(w, 0, "AA", [], 0, "AA", [], w.valid)
    

"""

from itertools import permutations


class World:

    def __init__(self, file_name, tmax=30):
        self.rates: dict[str, int] = {}
        self.exits: dict[str, list[str]] = {}
        self.paths: dict[(str, str), list[str]] = {}
        self.valid: list(str) = []
        self.tmax: int = tmax
        self.read_file(file_name)

    def read_file(self, file_name: str) -> dict[str, int]:
        with open(file_name) as f:
            # Look ma, no regex!
            for line in f.readlines():
                valve_name = line[6:8]
                rate = int(line.split(";")[0].split("=")[-1])
                if rate > 0:
                    self.valid.append(valve_name)
                if "lead to valves" in line:
                    exits = ((line.split("valves")[-1]).strip()).split(", ")
                else:
                    exits = [line.strip()[-2:]]

                self.rates[valve_name] = rate
                self.exits[valve_name] = exits

    def path(self, start: str, end: str) -> list[str]:
        try:
            return self.paths[(start, end)]
        except KeyError:
            res = self._find_path(start, end)
            self.paths[(start, end)] = res
            return res        

    def _find_path(self, start: str, end: str, visited=[]) -> list[str]:
        if start == end:
            return [start]
        this_exits = self.exits[start]
        if end in this_exits:
            return [start, end]
        sub_path = [self._find_path(ex, end, visited=(visited + [start])) 
                    for ex in this_exits if ex not in visited]
        # remove empty path
        sub_path = [(len(x), x) for x in sub_path if x != []]
        if sub_path == []:
            return []
        else:
            return [start] + min(sub_path)[1]

    def flow(self, route):
        """example: route = [(0, 'AA'), (2, 'BB'), (4, 'CC'), (6, 'DD'), (8, 'EE'), (12, 'HH'), (20, 'JJ')]
        """
        return sum(self.rates[pos] * (self.tmax - t) for t, pos in route)


def run(w: World):
    best_val, best_route = 0, []
    for dest in permutations(w.valid):
        t = 0
        route = []
        pos = "AA"
        for d in dest:
            t += len(w.path(pos, d))
            if t >= 30:
                break
            else:
                route.append((t, d))
            pos = d
        # route complete
        #print(route)
        val = w.flow(route)
        if val > best_val:
            best_val = val
            best_route = route
    return (best_val, best_route)

    
# Using permutations we will visit dead branches 
# if for exampke ABCxxxx take already too much time,
# there is no need to visit ABCDxxx and ABCExxx and so on

def optimize(
        w: World,
        t: int, pos: str, 
        route, still_closed: list[str],
        best_val: int=0, best_route=[]
    ):
    val = w.flow(route)
    if val > best_val:
        best_val = val
        best_route = route

    for v in still_closed:
        dt = len(w.path(pos, v))
        if t + dt >= w.tmax:
            continue
        else:
            new_t = t + dt
            new_route = route + [(t+dt, v)]
            new_pos = v
            new_closed = [x for x in still_closed if x != v]

            best_val, best_route = optimize(w, new_t, new_pos, new_route, new_closed, 
                                            best_val, best_route)
                                            
    return best_val, best_route
            

# part 2

def optimize2(
        w: World,
        t1: int, pos1: str, route1,
        t2: int, pos2: str, route2,
        still_closed: list[str],
        best_val: int=0
    ):
    val1 = w.flow(route1)
    val2 = w.flow(route2)
    if val1 + val2 > best_val:
        best_val = val1 + val2
        print("Val", best_val)
        print(route1)
        print(route2)

    is_in_bottom = route1 == [] and route2 == []

    for i, v in enumerate(still_closed):

        if is_in_bottom:
            # the bottom layer
            print("********************")
            print(i, "of", len(still_closed))

        for n in (1, 2):
            if n == 1:
                t1_ = t1 + len(w.path(pos1, v))
                pos1_ = v
                route1_ = route1 + [(t1_, pos1_)]
                t2_ = t2
                pos2_ = pos2
                route2_ = route2
            else:
                # cut search in half?
                t1_ = t1
                pos1_ = pos1
                route1_ = route1
                t2_ = t2 + len(w.path(pos2, v))
                pos2_ = v
                route2_ = route2 + [(t2_, pos2_)]

            if t1_ > w.tmax or t2_ > w.tmax:
                continue
            else:
                new_closed = [x for x in still_closed if x != v]
                best_val = optimize2(w, 
                        t1_, pos1_, route1_,
                        t2_, pos2_, route2_, 
                        new_closed, best_val)
                                            
    return best_val


def optimize2_b(
        w: World,
        t1: int, pos1: str, route1,
        t2: int, pos2: str, route2,
        still_closed: list[str],
        best_val: int=0
    ):
    val1 = w.flow(route1)
    val2 = w.flow(route2)
    if val1 + val2 > best_val:
        best_val = val1 + val2
        print("Val", best_val)
        print(route1)
        print(route2)

    is_in_bottom = route1 == [] and route2 == []

    for i, v in enumerate(still_closed):

        if is_in_bottom:
            # the bottom layer
            print("********************")
            print(i, "of", len(still_closed))

        if t1 < t2:
            t1_ = t1 + len(w.path(pos1, v))
            pos1_ = v
            route1_ = route1 + [(t1_, pos1_)]
            t2_ = t2
            pos2_ = pos2
            route2_ = route2
        else:
            # cut search in half?
            t1_ = t1
            pos1_ = pos1
            route1_ = route1
            t2_ = t2 + len(w.path(pos2, v))
            pos2_ = v
            route2_ = route2 + [(t2_, pos2_)]

        if t1_ > w.tmax or t2_ > w.tmax:
            continue
        else:
            new_closed = [x for x in still_closed if x != v]
            best_val = optimize2_b(w, 
                    t1_, pos1_, route1_,
                    t2_, pos2_, route2_, 
                    new_closed, best_val)
                                            
    return best_val






