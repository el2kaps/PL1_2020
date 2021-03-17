# Project     : Programming Languages 1 - Assignment 2 - Exercise 3
# Author(s)   : Eleni Elpida Kapsali (eleni_kaps@hotmail.com)
# Date        : May 31, 2020
# Description : Stayhome (Python code)
# -----------
# School of ECE, National Technical University of Athens.


import sys
#get neighbors in lexicographic order D,L,R,U
def Neighbors(world, i, j):
    neighbors = []
    if(world[i+1][j] != 'X'):
        neighbors.append([i+1, j])
    if(world[i][j-1] != 'X'):
        neighbors.append([i, j-1])
    if(world[i][j+1] != 'X'):
        neighbors.append([i, j+1])
    if(world[i-1][j] != 'X'):
        neighbors.append([i-1, j])
    return neighbors #list of neighbors of (i,j)

def Neighbors2(world, i, j):
    neighbors = []
    if(world[i+1][j] != 'X' and (isinstance(world[i+1][j], list) == False)):
        neighbors.append([i+1, j, 'D'])
    if(world[i][j-1] != 'X' and (isinstance(world[i][j-1], list) == False)):
        neighbors.append([i, j-1, 'L'])
    if(world[i][j+1] != 'X' and (isinstance(world[i][j+1], list) == False)):
        neighbors.append([i, j+1, 'R'])
    if(world[i-1][j] != 'X' and (isinstance(world[i-1][j], list) == False)):
        neighbors.append([i-1, j, 'U'])
    return neighbors #list of neighbors of (i,j)

#Open and read input
file = sys.argv[1]
world = [] #map of the world
with open(file) as f:
    for line in f:
        world.append(['X'] + list(line.rstrip()) + ['X'])

#world's dimensions
N = len(world) #number of lines
M = len(world[0])-2 #number of columns
#Put 'X' in margins
margins = ['X' for x in range(M+2)]
world.insert(0,margins)
world.append(margins)

#find W and apply the first Fill-Flood
#when the first Flood-Fill ends the world's
#map contains the time when Covid reached each place
queue1 = []
for i in range(1, N+1):
    for j in range(1, M+1):
        if(world[i][j] == 'W'):
            world[i][j] = 0 
            queue1.append([i,j,0])
            
covid_in_airport = False
infect_airports = 0
countdown = -1
while(queue1 or countdown >= 0):
    queue2 = []
    if(covid_in_airport == True and countdown > 0):
        countdown = countdown - 1
    elif(covid_in_airport == True and countdown == 0):
        countdown = -1
        for i in range(1, N+1):
                for j in range(1, M+1):
                    if(world[i][j] == 'A'):
                        world[i][j] = infect_airports
                        queue2.append([i,j,infect_airports])
    while(queue1):
        curr = queue1.pop(0)
        curr_i = curr[0]
        curr_j = curr[1]
        curr_t = curr[2]
        next_time = curr_t + 2
        for i in Neighbors(world, curr_i, curr_j):
            neigh_i = i[0]
            neigh_j = i[1]
            s = world[neigh_i][neigh_j]
            if(s == '.' or s == 'A' or s == 'S' or s == 'T'):
                if(s == 'S'):
                    S_coordinates = [neigh_i, neigh_j]
                elif(s == 'T'):
                    T_coordinates = [neigh_i, neigh_j]
                else:
                    if(s == 'A' and covid_in_airport == False):
                        covid_in_airport = True
                        infect_airports = next_time + 5
                        countdown = 2 #in every while repeat time increases
                                      #per two units
                world[neigh_i][neigh_j] = next_time
                queue2.append([neigh_i, neigh_j,next_time])
    queue1 = queue2

#Apply again Flood-Fill. Begin from S.
#If arrive in [i][j] in time < world[i][j]
#update world[i][j]=[time, parent's coordinates, move]
queue1 = [] 
queue1.append(S_coordinates)
i_S = S_coordinates[0]
j_S = S_coordinates[1]
world[i_S][j_S] = 'X' 

counter = 0
stayhome = False
while(queue1 and stayhome == False):
    counter = counter + 1
    queue2 = []
    while(queue1):
        curr = queue1.pop(0)
        curr_i = curr[0]
        curr_j = curr[1]
        for i in Neighbors2(world, curr_i, curr_j):
            neigh_i = i[0]
            neigh_j = i[1]
            s = world[neigh_i][neigh_j]
            if(s > counter):
                if(neigh_i == T_coordinates[0] and neigh_j == T_coordinates[1]):
                    stayhome = True
                world[neigh_i][neigh_j] = [curr_i, curr_j, i[2]]
                queue2.append([neigh_i, neigh_j])
    queue1 = queue2
                               
if(stayhome == False):
    print('IMPOSSIBLE')
else:
    print(counter)
    path = []
    i = T_coordinates[0]
    j = T_coordinates[1]
    while(True):
        if(i == i_S and j == j_S):
            break
        path.append(world[i][j][2])
        i_aux = i
        i = world[i][j][0]
        j = world[i_aux][j][1]
    path.reverse()
    print(''.join(path))                        


