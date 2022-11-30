#2.a
y=c(1,2,3,2,0,0,1,4,2)
curve(dgamma(x,sum(y),9),to=5)



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


#2.b
a <- function(lambda) {dpois(0:10,lambda)} # Child dist.

G <- Vectorize(function(s,lambda) 
  {
  a=dpois(0:10,lambda)
  sum(a[1:length(a)]*s^(0:(length(a)-1)))
  })

GS <- Vectorize(function(x) {abs(G(x)-x)})

get_extintion_p = Vectorize(function(lambda) {
  optimize(Vectorize(function(x) {abs(G(x,lambda)-x)}), interval =c(0,0.99))$minimum
})


get_extintion_p = Vectorize(function(lambda) {
  
  
  optimize(Vectorize(function(x) {abs(G(x,lambda)-x)}), interval =c(0,0.99))$minimum
})



# smallest root
#root <- uniroot(GS, lower=0, upper=1) 
#optimize(GS,interval = c(0,0.99))
#curve(GS(x))


#2.c
y=c(1,2,3,2,0,0,1,4,2)
prior <-function(x) {dgamma(x,sum(y),9)}

g_x <- function(p) dgamma(p,sum(y),9)*Vectorize(unc)(p)
integrate(g_x, 1/10, 1)$value/integrate(g_x, 0, 1)$value



i1=integrate(Vectorize(unc), 0, Inf)$value


#2.d
p <- seq(0, 10, length.out=500)
posterior <- dgamma(p,sum(y),9)


ext_list = c()

for (i in 1:1000){
  lambda_sample <- sample(p, 1, prob = posterior)
  ext_list[i]=get_extintion_p(lambda_sample)
}

mean(ext_list)



