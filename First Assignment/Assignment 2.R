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
expected_throws = b[1]*p+b[2]*p+b[3]*p+b[4]*b[5]*0+1


#b
P<-matrix(c(F[1,1],0,0,0,0,
            0, F[2,2],0,0,0,
            0,0,F[3,3],0,0,
            0,0,0,F[4,4],0,
            0,0,0,0,F[5,5]),5,5) #0
#S<-diag(5)-solve(P)%*%Q
H<-(F-diag(5))%*%solve(P)
p_3 <- sum(1/4*H[c(1,2,4),3])+1/4 

#c
p_0 = (1/4*(1-H[1,3])+1/4*(1-H[2,3])+1/4*(1-H[4,3])) # Chance to never visit 3
p_1 = (1/4)+((1/4*H[1,3])+1/4*(H[2,3])+1/4*(H[4,3]))*(1-H[3,3]) # Chance to visit 3 once
p_3_more = 1-(p_0+p_1)