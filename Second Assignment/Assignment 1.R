library(LearnBayes)

## a) Compute the expectation of the posterior for P

# 10 experiments, 3 ones, 4 twos, 3 threes
# Pi ∼ Dirichlet(1,1,1)

# Transition matrix
P1 = c(0, 2, 1)
P2 = c(1, 0, 2)
P3 = c(1, 2, 0)

# E(Pi | data)
EP1 = (c(1, 1, 1) + P1) / (length(P1) + P1)
EP2 = (c(1, 1, 1) + P2) / (length(P2) + P2)
EP3 = (c(1, 1, 1) + P3) / (length(P3) + P3)
  
# Expectation of the posterior for P
matrix(c(EP1, EP2, EP3), 3, 3, byrow = TRUE)

