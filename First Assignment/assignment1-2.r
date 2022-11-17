p<-1/4
mdata<-c(0,p,p,p,p,0,
         0,0,p,p,p,p,
         p,0,0,p,p,p,
         0,0,0,0,p,p,
         1,0,0,0,0,0,
         0,0,0,0,0,1)
m<-matrix(mdata,6,6,byrow = TRUE)
a<-c(p,p,p,p,0,0)

#a
Q<-m[0:5,0:5]
R<-m[0:5,6]
O<-m[6,0:5]
I<-diag(5)
F=solve(I-Q) # Solve gives inverse 
#sum(F[1,])

b<-F%*%c(1,1,1,1,1)
tot_throws = b[1]*p+b[2]*p+b[3]*p+b[4]*b[5]*0+1

#b
a<-c(p,p,p,p,0,0)

Q<-m[c(1,2,4,5,6),c(1,2,4,5,6)]
R<-m[c(1,2,4,5,6),3]
O<-c(0,0,0,0,0)
I<-diag(5)
F=solve(I-Q)

get_chance <- function(n) {
  3/4*sum(F[n,c(1,2,4,5)])/sum(F[n,1:5])
}

1-(3/4*sum(F[1,c(1,2,4,5)])/sum(F[1,1:5])*sum(F[1,c(1,2,4,5)])/sum(F[1,1:5])*sum(F[1,c(1,2,4,5)])/sum(F[1,1:5])*sum(F[1,c(1,2,4,5)])/sum(F[1,1:5]))
