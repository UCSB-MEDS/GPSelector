##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                               ~~
##        LINEAR INTEGER PROGRAMMING REFERS TO THE CLASS OF COMBINATORIAL      ----
## CONSTRAINED OPTIMIZATION PROBLEMS WITH INTEGER VARIABLES, WHERE THE OBJECTIVE ~~
##  FUNCTION IS A LINEAR FUNCTION AND THE CONSTRAINTS ARE LINEAR INEQUALITIES.   ~~
##                                                                               ~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# MORE SIMPLY: method to achieve the best outcome (such as maximum profit or lowest cost) in a mathematical model whose requirements are represented by linear relationships
# see this paper: https://cit.iict.bas.bg/CIT_2011/v11-1/3-25.pdf

#..........................load packages.........................
# library(readxl)
library(tidyverse)
library(lpSolve)

#..........................read in data..........................
# points <- read_csv(here::here("MEDS_2021_22", "MEDS_capstone_2022.csv")) # should be N x k matrix where N is number of students and k is num of projects
# proj_maximums <- read_csv(here::here("MEDS_2021_22", "project_maximums.csv")) # should be k x 1 matrix

# FAKE DATA
points <- read_csv(here::here("test2021", "made_up", "GP2021_test9.csv"))
proj_maximums <- read_csv(here::here("test2021", "made_up", "GP2021_test9_projMax.csv"))

#..............drop names, convert points to matrix..............
fmat <- points %>% 
  select(-c("Last Name", "First Name")) %>% 
  as.matrix()

#................convert max group sizes to matrix...............
B <- proj_maximums %>% 
  select(Points) %>% # the 'Points' col refers to the max number of group members
  as.matrix()

#..........get number of students and project proposals..........
k <- dim(B)[1] # project proposals
N <-  dim(fmat)[1] # students

#....................check that matrices match...................
if (dim(fmat)[2] != k){
  print("Matrices are not entered correctly")
}

#..........transpose points, gather into single column...........
f <- t(fmat) %>% 
  as.data.frame() %>% 
  gather(Project, Points) %>% 
  select(Points)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ updated version of the above code using pivot_* (but values begin to differ? don't use this for now)  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# f <- t(fmat) %>% 
#   as.data.frame() %>% 
#   pivot_longer(cols = everything(), names_to = "Project", values_to = "Points") %>% # was gather()
#   arrange(Project) %>% 
#   select(Points)

#........construct a diagonal matrix with dimensions of k........
A0 <- diag(1, k) # k = 22, in the case of the 2017 class or 9 in the case of MEDS 2021-2022
A <- A0

#........use cbind on A and A0 to turn back into a matrix?........
for (i in 1:(N-1)){ # from 1 to the length of students - 1 (84, in the case of the 2017 class)
  A <- cbind(A, A0)
}

#..........create matrix of 0s with dimensions N x N*k..........
# these are the student points?? (from JC discussion, but not sure I see that)
Aeq <- matrix(data = 0, nrow = N, ncol =  N*k)

#................not sure what's going on here tbh...............
for (i in 1:N) {
  Aeq[i, (i*k-k+1):(i*k)] = 1
}

#............create matrix of 1s with dimensions N x 1...........
Beq <- matrix(data = 1, nrow = N, ncol = 1)

#......................join A and A0 by rows.....................
const.mat <- rbind(A, Aeq)

#..create a vector of repeating <= equal to k and == equal to N..
const.dir <-  c(rep("<=", k), rep("==", N))

#.....................join B and Beq by rows.....................
const.rhs <- rbind(B, Beq)

#........lp() is an interface to lp_solve, a MILP solver.........
solution <- lp(direction = "max", objective.in = f, const.mat = const.mat, const.dir = const.dir, const.rhs = const.rhs, all.bin = T)

#..............combine names and algorithm results...............
sol_out <- cbind(points$`Last Name`, points$`First Name`, matrix(solution$solution, N, k, byrow = T))
colnames(sol_out) <- colnames(points)

#..........................write to csv..........................
write.csv(x = sol_out, file = "Solutions.csv", row.names = F)

