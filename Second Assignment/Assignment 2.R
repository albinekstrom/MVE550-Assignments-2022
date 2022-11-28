#2.a
y=c(1,2,3,2,0,0,1,4,2)
curve(dgamma(x,sum(y),9),to=5)
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
#2.b
