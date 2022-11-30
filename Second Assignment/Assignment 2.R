#2.a
y=c(1,2,3,2,0,0,1,4,2)
#p <- seq(0, 10, length.out=1001)
curve(dgamma(x,sum(y),9),to=5)
#plot(p,dgamma(p,sum(y),9),to=5,type="l") # Posterior observed y
#lines(p,dgamma(p,sum(y)+p,9+1),to=5,col="green") # postyerior observer y_11
#2.a 2nd version

get_prior <- function(z,p)
{
  dgamma(p,sum(),length(z))
}

get_lh <- function(z,p)
{
  likelyhood = 1
  for (i in 1:length(z))
    {
      likelyhood = likelyhood*dpois(z[i],p)
  }
  return(likelyhood)
}


p <- seq(0, 10, length.out=1001)

# Children per generation
z=list(c(1),
       c(2),
       c(3,2),
       c(0,0,1,4,2))

zi=unlist(z[1])
prior=dgamma(p,sum(zi),length(zi))
for (i in 1:3){
  zi=unlist(z[i])
  for (i in 1:length(zi))
  {
  likelyhood=dpois(zi,p)
  }
  
  posterior=prior*likelyhood
  plot(posterior,x = p)
  prior=posterior
  
}


n <- 3
# Set up plot
maxsize = 20
maxchildren=3
a=c(1/4,1/4,1/4,1/4)
plot(1:n, c(0, rep(maxsize, n-1)), type="n", 
     xlab="generation", ylab="Gen.size")

z=list(c(1),
       c(1,2),
       c(3,2),
       c(0,0,1,4,2))

z=c()
#2.b

result <- c(1,2,5,7)
for (i in 2:n) { # generation
  for (j in 1:result[i-1]){ # parent
    children <- unlist(z[i])[j]
      for (k in 1:children){
        result[i] <- result[i] + 1 # childs height?
        lines(c(i-1,i), c(j, result[i]))
      }
    }
}






