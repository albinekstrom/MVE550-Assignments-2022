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


n <- 4
# Set up plot
maxsize = 7
maxchildren=3
a=c(1/4,1/4,1/4,1/4)
plot(1:n, c(0, rep(maxsize, n-1)), type="n", 
     xlab="generation", ylab="Gen.size")

z=list(c(1),
       c(1,2),
       c(3,2),
       c(0,0,1,4,2))

# 2nd gen
parent <- c(2,1)
child1 <- c(3,parent[2])
child2 <- c(3,parent[2]+1)


lines(parent,child1)
lines(c(parent[1],child1[1]),parent)

lines(c(parent[1],child1[1]), c(parent[2],child1[2]))
lines(c(parent[1],child2[1]), c(parent[2],child2[2]))

nr_gen <- 4
result <- c(1,2,5,7)
for (g in 2:nr_gen-2)
{
  lines(c(g,g-1), c(result[g-1],result[g]))
}


#2.b
a <- function(lambda) {dpois(0:10,lambda)}

# average child per woman in Sweden according to SCB
lambda <- 1.67 
G <- Vectorize(function(s) 
  {
  a=a(lambda)
  sum(a[1:length(a)]*s^(0:(length(a)-1)))
  })

GS <- Vectorize(function(x) {G(x)-x})

# smallest root
root <- uniroot(GS, lower=0, upper=1) 
curve(GS(x))


#2.c
y=c(1,2,3,2,0,0,1,4,2)
posterior <- dgamma(1:10,sum(y),9)
lambda_samples <- sample(1:10, 1000, replace = TRUE, prob = posterior)

# function to calculate extinction prob.
min_root <- function(lambda) {
    a <- dpois(0:10,lambda)
    G <- Vectorize(sum(a[1:length(a)]*s^(0:(length(a)-1))))
    }

# average child per woman in Sweden according to SCB
lambda <- 1.67 
G <- Vectorize(function(s) 
{
  a=a(lambda)
  sum(a[1:length(a)]*s^(0:(length(a)-1)))
})

GS <- Vectorize(function(x) {G(x)-x})
min_root <- uniroot(GS(s), lower=0, upper=1)
#mean <- sum(s)/length(s)





