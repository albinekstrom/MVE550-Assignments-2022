data = read.table("C:/Skola/MVE550/repo/MVE550-Assignments-2022/Third Assignment/dataAssignment3.txt", header=TRUE)
x = unlist(data["x"],use.names = FALSE)
y = unlist(data["y"],use.names = FALSE)
z = unlist(data["z"],use.names = FALSE)


sick<-which(z==1)
plot(x,y)
points(x[sick],y[sick],col='red')

t_1 <- seq(0, 10, length.out=50)
t_2 <- seq(0, 10, length.out=50)
t_3 <- seq(0, 10, length.out=50)


disease_p <- function(x,y,theta_1,theta_2,theta_3){
  a=exp(exp(theta_1)*x + exp(theta_2)*((y-theta_3)^2))
  return((a-1)/(a+1))
}

Likelyhood <- function(x,y,theta_1,theta_2,theta_3){
  lh = 1
  for (i in length(x)) {
  lh = lh * disease_p(x[i],y[i],theta_1,theta_2,theta_3)
}
  return(lh)
}

Posterior <- function(x,y){
  lh = 1
  theta = c()
  theta[1] = runif(1)
  theta[2] = runif(1)
  theta[3] = runif(1,10,25)
  lh = Likelyhood(x,y, theta[1],theta[2],theta[3])
  
  return(c(lh*theta[1],lh*theta[2],lh*theta[3]))
}

post_th <- function(x,y,theta){
  lh = Likelyhood(x,y, theta[1],theta[2],theta[3])
  
  return(c(lh*theta[1],lh*theta[2],lh*theta[3]))
}

#b
Posterior(x,y)
# post_th(x,y,c(1,2,25))

