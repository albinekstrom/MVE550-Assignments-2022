data = read.table("C:/Skola/MVE550/repo/MVE550-Assignments-2022/Third Assignment/dataAssignment3.txt", header=TRUE)
x = unlist(data["x"],use.names = FALSE)
y = unlist(data["y"],use.names = FALSE)
z = unlist(data["z"],use.names = FALSE)


healthy<-which(-(z-1) ==1)
plot(x,y)
points(x[sick],y[sick],col='red')

f_d <- function(x_d,y_d,theta_1,theta_2,theta_3){
  a <- exp(exp(theta_1)*x_d + exp(theta_2)*((y_d-theta_3)^2))
  #if((a-1)/(a+1)>1){}
  return ((a-1)/(a+1))
  
}

Likelihood <- function(theta){
  
  lh = f_d(x,y,theta[1],theta[2],theta[3])
  lh[healthy] = 1-lh[healthy]
  return(prod(lh))
}
Likelihood_test <- function(theta){
  lh = f_d(x,y,theta[1],theta[2],theta[3])
  lh[healthy] = 1-lh[healthy]
  return(lh)
}
Likelihood_test_2 <- function(theta){
  lh = f_d(x,y,theta[1],theta[2],theta[3])
  return(lh)
  
}



post <- function(theta){
  return(log(Likelihood(theta)))
}

log_a <- function(theta){
  loga <- log(Likelihood(theta))
  return(loga)
}



#b

# post_th(x,y,c(1,2,25))
post(c(1,1,20))

#c
# Simulate from the posterior
N <- 10000
result <- matrix(0,N,3)
#Initialize some starting theta: 
result[1,] <- c(-4, -2, 16)
result[1,] <- c(-1.281, -21.559, 215.430)
print(c("Start LH", Likelihood(result[1,])))
prop=c()
for (i in 2:N) {
  for(j in 1:3){prop[j] <- result[i-1,j] + rnorm(1,0,0.4)}
  a <-  post(prop) - post(result[i-1,])
  
  if (runif(1)<exp(a)){
    result[i,] <- prop
    #print(c("accepted",exp(a)))
  }
  else{
    result[i,] <- result[i-1,]
    #print(c("declined",exp(a)))
  }
  #print(c("Likelyhood",Likelihood(result[i,])))
  
}

print(c("End LH", Likelihood(result[N,])))


#plot(result[,1],type='l')
#plot(result[,2],type='l',col='red')
#plot(result[,3],type='l',col='blue')

f_d(x[1],y[1],result[N,1],result[N,2],result[N,3])
