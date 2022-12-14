data = read.table("C:/Skola/MVE550/repo/MVE550-Assignments-2022/Third Assignment/dataAssignment3.txt", header=TRUE)
x = unlist(data["x"],use.names = FALSE)
y = unlist(data["y"],use.names = FALSE)
z = unlist(data["z"],use.names = FALSE)


sick<-which(z==1)
plot(x,y)
points(x[sick],y[sick],col='red')

f_d <- function(x,y,theta_1,theta_2,theta_3){
  a=exp(exp(theta_1)*x + exp(theta_2)*((y-theta_3)^2))
  return((a-1)/(a+1))
}

Likelihood <- function(theta){
  return(prod(f_d(x,y,theta[1],theta[2],theta[3])))
}


post <- function(theta){
  return(log(Likelihood(theta)))
}





#b

# post_th(x,y,c(1,2,25))
post(c(1,1,20))

#c
# Simulate from the posterior
N <- 10000
result <- matrix(0,N,3)
#Initialize some starting theta: 
result[1,] <- c(1, 1, 15)
for (i in 2:N) {
  prop <- rnorm(3,result[i-1,],0.4)
  while(is.nan(post(prop))){
    prop <- rnorm(3,result[i-1,],0.4)
  }
  print(prop)
  print(post(prop))
  
  if (runif(1)<post(prop)/post(result[i-1,]))
    result[i,] <- prop
  else
    result[i,] <- result[i-1,]
}
plot(result[,1],type='l')
lines(result[,2],col='red')
lines(result[,3],col='blue')




