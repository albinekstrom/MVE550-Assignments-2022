data = read.table("./Third Assignment/dataAssignment3.txt", header=TRUE)
x = unlist(data["x"],use.names = FALSE)
y = unlist(data["y"],use.names = FALSE)
z = unlist(data["z"],use.names = FALSE)

healthy <- which(z == 0)
plot(x,y)
sick <- which(z==1)
points(x[sick],y[sick],col='red')

# Probability density function
f_d <- function(x_d,y_d,theta_1,theta_2,theta_3){
  a <- exp(exp(theta_1)*x_d + exp(theta_2)*((y_d-theta_3)^2))
  return ((a-1)/(a+1))
}

## a)

# Likelihood function
Likelihood <- function(theta){
  lh = f_d(x,y,theta[1],theta[2],theta[3])
  lh[healthy] = 1-lh[healthy]
  return(prod(lh))
}

Posterior <- function(theta){
  c = 1 # prior is flat and proportional to constant
  return (Likelihood(theta)*c)
}

## b)
post <- function(theta){
  lh = f_d(x,y,theta[1],theta[2],theta[3])
  lh[healthy] = 1-lh[healthy]
  return( sum( log(lh) ) )
}

post_log <- function(theta){
  return(log(Posterior(theta)))
}

## c)
# Simulate from the posterior
N <- 10000
result <- matrix(0,N,3)

#Initialize some starting theta: 
result[1,] <- c(-2, -4, 16)
print(c("Start LH", Likelihood(result[1,])))
prop=c()

for (i in 2:N) {
  # Generate 3 completely different new theta
  for(j in 1:3){prop[j] <- result[i-1,j] + rnorm(1,0,0.4)}
  a <-  post(prop) - post(result[i-1,])
  if (runif(1)<exp(a)){
    result[i,] <- prop
  }
  else{
    result[i,] <- result[i-1,]
  }
  print(c("Likelyhood",Likelihood(result[i,])))
}

print(c("End LH", Likelihood(result[N,])))
print(c("Result", result[N,]))

#plot(result[,3],type='b',col='dark green',     main = "Theta 3")
#plot(result[,2],type='l',col='red')
#plot(result[,3],type='l',col='blue')

#theta <- colMeans(result)
approx <- f_d(x,y,result[N,1],result[N,2],result[N,3])
#approx <- f_d(x,y,theta[1],theta[2],theta[3])
sum(abs(approx-z))


## d) 
t <- result[N,]
ch <- f_d(3,13,t[1],t[2],t[3]) # Prop at x=3 and y=13
dbinom(9,10,ch) # Prop of 9 of 10 animals get sick
