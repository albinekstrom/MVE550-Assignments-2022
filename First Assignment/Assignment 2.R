## a) Expected number of dice throws

p<-1/4
         #1,2,3,4,6
mdata<-c(p,p,p,p,0,   #1
         p,0,p,p,p,   #2
         2*p,0,0,p,p, #3
         2*p,p,0,0,p, #4
         0,0,0,0,1)   #6
m<-matrix(mdata,5,5,byrow = TRUE)

Q<-m[0:4,0:4]
R<-m[0:4,5]
O<-m[5,0:4]

I<-diag(4)
F=solve(I-Q)
NOF <- F%*%c(1,1,1,1) # Number of throws
throws <- 1/4*(NOF[1] + NOF[2] + NOF[3] + NOF[4]) + 1 # Total throws
cat("Expected number of throws: ", throws)


## b) Probability that the player will visit state 3 

P<-matrix(c(F[1,1],0,0,0,
            0, F[2,2],0,0,
            0,0,F[3,3],0,
            0,0,0,F[4,4]),4,4)
H <- (F-diag(4))%*%solve(P)
prob <- sum(1/4*H[c(1,2,4),3])+1/4 
cat("Probability of visiting state 3: ", prob)


## c) Probability that the player will visit state 3 two or more

p_0 <- (1/4*(1-H[1,3])+1/4*(1-H[2,3])+1/4*(1-H[4,3])) # Chance to never visit 3
p_1 <- (1/4)+((1/4*H[1,3])+1/4*(H[2,3])+1/4*(H[4,3]))*(1-H[3,3]) # Chance to visit 3 once
prob_2_more <- 1-(p_0+p_1)
cat("Probability of visiting state 3 two or more times: ", prob_2_more)



