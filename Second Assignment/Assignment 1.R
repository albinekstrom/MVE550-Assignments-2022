library(LearnBayes)

## a) Compute the expectation of the posterior for P

# 10 experiments, 3 ones, 4 twos, 3 threes
# Pi ∼ Dirichlet(1,1,1)

# Functions
get_tmatrix <- function(y)
{
  m = matrix(0, nrow = 3,ncol = 3,byrow = T)
  for (i in 1:length(y)-1)
  {
    v1=y[i]
    v2=y[i+1]
    m[v1,v2]=m[v1,v2]+1
  }
  return (m)
}

get_expected_m <- function(y)
{
  m = get_tmatrix(y)
  P1 = m[1,]
  P2 = m[2,]
  P3 = m[3,]
  
  # E(Pi | data)
  EP1 = (c(1, 1, 1) + P1) / (length(P1) + sum(P1))
  EP2 = (c(1, 1, 1) + P2) / (length(P2) + sum(P2))
  EP3 = (c(1, 1, 1) + P3) / (length(P3) + sum(P3))
  
  # Expectation of the posterior for P
  return(matrix(c(EP1, EP2, EP3), 3, 3, byrow = TRUE))
}

simulate1 <- function(y){
  m=get_expected_m(y)
  for (i in (length(y)+1):400)
  {
    t = tail(y,1)
    y[i]=sample(c(1,2,3),1,prob = m[t,])
    m=get_expected_m(y)
    hist(y)
    #Sys.sleep(0.05)
  }
  return (y)
}

simulate1 <- function(y){
  m=get_expected_m(y)
  for (i in (length(y)+1):400)
  {
    t = tail(y,1)
    y[i]=sample(c(1,2,3),1,prob = m[t,])
    m=get_expected_m(y)
    #hist(y)
    #Sys.sleep(0.05)
  }
  return (y)
}

simulate2 <- function(y){
  m=get_expected_m(y)
  for (i in (length(y)+1):400)
  {
    t = tail(y,1)
    y[i]=sample(c(1,2,3),1,prob = m[t,])
    m=get_expected_m(y)
    #hist(y)
    #Sys.sleep(0.05)
  }
  return (m[1,2])
}

simulate3 <- function(y){
  m1 = get_expected_m(y)
  m = m1
  for (i in (length(y)+1):400)
  {
    t = tail(y,1)
    y[i]=sample(c(1,2,3),1,prob = m1[t,])
    m=get_expected_m(y)
  }
  return (m[1,2])
}

simulate4 <- function(y){
  m1 = get_expected_m(y)
  m = m1
  for (i in (length(y)+1):400)
  {
    t = tail(y,1)
    Psamp = sample(1,)
    y[i]=sample(c(1,2,3),1,prob = m1[t,])
    m=get_expected_m(y)
  }
  return (m[1,2])
}


# 1.a
# observed values
y = c(1,2,3,2,3,1,2,1,3,2)

P = get_expected_m(y)
P1 = P[1,]
P2 = P[2,]
P3 = P[3,]

# How to get E(P|y)

# 1.b

simulate1(y)
hist(y)

# 1.c
expected_p12 = replicate(1000,simulate2(y))
hist(expected_p12)

# 1.d
expected_p12 = replicate(1000,simulate3(y))
hist(expected_p12)

# 1.e


#m_count = matrix(c(length(which(y == 1)),
#         length(which(y == 2)),
#         length(which(y == 3))),
#         1,3,byrow = T)

