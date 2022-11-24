library(LearnBayes)

## a) Compute the expectation of the posterior for P

# 10 experiments, 3 ones, 4 twos, 3 threes
# Pi ∼ Dirichlet(1,1,1)

# Transition matrix
P1 = c(0, 2, 1)
P2 = c(1, 0, 2)
P3 = c(1, 2, 0)

# E(Pi | data)
EP1 = (c(1, 1, 1) + P1) / (length(P1) + sum(P1))
EP2 = (c(1, 1, 1) + P2) / (length(P2) + sum(P2))
EP3 = (c(1, 1, 1) + P3) / (length(P3) + sum(P3))
  
# Expectation of the posterior for P
m = matrix(c(EP1, EP2, EP3), 3, 3, byrow = TRUE)

y=c(1,2,3,2,3,1,2,1,3,2)

for (i in (length(y)+1):20)
{
  t = tail(y,1)
  y[i]=sample(c(1,2,3),1,prob = m(t,))
  
}