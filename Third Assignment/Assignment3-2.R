generate_trees <- function(lambda){
  p = rpois(1,lambda)[1]
  x=runif(p,0,1)
  y=runif(p,0,1)
  m = matrix(c(x,y),2,p)
  return(m)
}

calc_z <- function(m){
  x=m[1,]
  y=m[2,]
  K=length(m[1,])
  min_v = c()
  for(i in 1:K){
    dist = c()
    for(j in 1:K){
      if(i==j){
        dist[j]=999}
      else{
        dist[j] = sqrt((x[i]-x[j])^2+(y[i]-y[j])^2) # point dist
      }
      
    }
    min_v[i]=min(dist)
  }

  # print(sum(min_v)*(1/K))
  return (mean(min_v))
  
}




generate_pois <- function(tmin,tmax){
  v = c()
  p = rpois(1,36)[1]
  x=runif(p,0,1)
  y=runif(p,0,1)
    
  for(i in 1:p){
    if(tmin <= x[i] & x[i] <= tmax & tmin <= y[i] & y[i] <= tmax) {v[i]=1}
    else {v[i]=0}
  }
  if(sum(v)>=6){return (1)}
  else {return (0)}
}

prob <- function(C, lambda, n) {
  return ((exp(-lambda*C)*(lambda*C)^n)/factorial(n))
}

# 1a
C <- abs(0.2-0.6)*abs(0.2-0.6)
C <- 0.16 # [0.2,0.6] x [0.2,0.6] => 0.4 x 0.4
lambda <- 36

# Probability of more or equal to 6 trees
1 - (prob(C, lambda, 5)+prob(C, lambda, 4)
     +prob(C, lambda, 3)+prob(C, lambda, 2)
     +prob(C, lambda, 1)+prob(C, lambda, 0))

C1 <- abs(0.2-0.6)*abs(0.2-0.6)
C2 <- abs(0.4-0.8)*abs(0.4-0.8)
prob(C1, lambda, 4)*prob(C2, lambda, 4)

# 2b Probability of exactly 4 in each square
Ci <- abs(0.4-0.6)*abs(0.4-0.6)
C1 <- abs(0.2-0.6)*abs(0.2-0.6) - Ci
C2 <- abs(0.4-0.8)*abs(0.4-0.8) - Ci
prob(Ci, lambda, 0)*prob(C1, lambda, 4)*prob(C2, lambda, 4)+
prob(Ci, lambda, 1)*prob(C1, lambda, 3)*prob(C2, lambda, 3)+
prob(Ci, lambda, 2)*prob(C1, lambda, 2)*prob(C2, lambda, 2)+
prob(Ci, lambda, 3)*prob(C1, lambda, 1)*prob(C2, lambda, 1)+
prob(Ci, lambda, 4)*prob(C1, lambda, 0)*prob(C2, lambda, 0)

# 2c
generate_trees(lambda=36)
# t = replicate(1000, generate_pois(0.2,0.6)) # Sample 1a
# sum(t)/length(t)


# 2d
lambda=rgamma(1,36,1)
t = generate_square(lambda)
plot(t[1,],t[2,],pch=2)

# 2e
generate_square(36)



