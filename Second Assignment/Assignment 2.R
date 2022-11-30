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

z=c()

n <- 10
lambda <- 0.7
G <- Vectorize(function(s) {sum(s^(0:n)*dpois(0:n,lambda))})

s <- seq(0, 1, length.out=1001)
plot(c(0,1), c(0,1), type="n", xlab="s", ylab="G(s)")
lines(s, G(s))

optimize(G(s)-s==0,interval = c(0,1))
#abline(a=0, b=1)

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

G <- Vectorize(function(s,lambda=2) 
  {
  a=a(lambda)
  sum(a[1:length(a)]*s^(0:(length(a)-1)))
  })

GS <- Vectorize(function(x) {G(x)-x})

#2.c







