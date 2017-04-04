### Replicating the costellization function in R

## Read the data

library(readxl)
library(tidyverse)
library(lpSolve)

file_path <- "GP2017jc.xlsx"

points <-  read_excel(file_path, sheet = "Point Allocations", col_names = T)

fmat <- dplyr::select(points, -c(1,2)) %>% 
  as.matrix()

B <-  read_excel(path = file_path, sheet = "Project Maximums", col_names = T) %>%
  select(Points) %>% 
  as.matrix()

k <- dim(B)[1] # Number of projects
N <-  dim(fmat)[1]
if (dim(fmat)[2] != k){
  print('Matrices are not entered correctly')
}

f <- t(fmat) %>% 
  as.data.frame() %>% 
  gather(Project, Points) %>% 
  select(Points)

A0 <- diag(1, k)
A <- A0

for (i in 1:(N-1)){
  A <- cbind(A, A0)
}

Aeq <- matrix(0, N, N*k)

for (i in 1:N) {
  Aeq[i, (i*k-k+1):(i*k)] = 1
}

Beq <- matrix(1, N, 1)

const.mat <- rbind(A, Aeq)
const.dir <-  c(rep("<=", k), rep("==", N))
const.rhs <- rbind(B, Beq)

solution <- lp(direction = "max", objective.in = f, const.mat = const.mat, const.dir = const.dir, const.rhs = const.rhs, all.bin = T)

sol_out <- cbind(points$`Last Name`, points$`First Name`, matrix(solution$solution, N, k, byrow = T))
colnames(sol_out) <- colnames(points)

write.csv(x = sol_out, file = "Solutions.csv", row.names = F)


