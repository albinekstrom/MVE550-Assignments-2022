p<-1/4
mdata<-c(p,p,p,p,0,   #1
         p,0,p,p,p,   #2
         2*p,0,0,p,p, #3
         2*p,p,0,0,p, #4
         0,0,0,0,1)   #6
m<-matrix(mdata,5,5,byrow = TRUE)


#a
Q<-m[0:4,0:4]
R<-m[0:4,5]
O<-m[5,0:4]
I<-diag(4)
F=solve(I-Q) # Solve gives inverse 
#sum(F[1,])

b<-F%*%c(1,1,1,1)
expected_throws = p*(b[1]+b[2]+b[3]+b[4])+1


#b
P<-matrix(c(F[1,1],0,0,0,
            0, F[2,2],0,0,
            0,0,F[3,3],0,
            0,0,0,F[4,4]),4,4) #0
#S<-diag(5)-solve(P)%*%Q
H<-(F-diag(4))%*%solve(P)
p_3 <- sum(1/4*H[c(1,2,4),3])+1/4 

#c
p_0 = (1/4*(1-H[1,3])+1/4*(1-H[2,3])+1/4*(1-H[4,3])) # Chance to never visit 3
p_1 = (1/4)+((1/4*H[1,3])+1/4*(H[2,3])+1/4*(H[4,3]))*(1-H[3,3]) # Chance to visit 3 once
p_2_more = 1-(p_0+p_1)